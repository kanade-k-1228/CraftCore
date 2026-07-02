use std::path::{Path, PathBuf};

use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use lsp_types::notification::{
    DidChangeConfiguration, DidChangeTextDocument, DidOpenTextDocument, DidSaveTextDocument,
    Notification as _, PublishDiagnostics,
};
use lsp_types::request::{DocumentSymbolRequest, GotoDefinition, HoverRequest, Request as _};
use lsp_types::{
    DocumentSymbolResponse, GotoDefinitionResponse, HoverProviderCapability, InitializeParams,
    OneOf, PublishDiagnosticsParams, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, Url,
};

use crate::analysis::{build_snapshot, diagnostics};
use crate::convert::{path_to_url, url_to_path};
use crate::features;
use crate::state::ServerState;

/// initialize ハンドシェイクからメインループまでを実行する。
/// stdio / memory どちらの Connection でも動く (テストは memory を使う)。
pub fn run(connection: Connection) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
    let capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        definition_provider: Some(OneOf::Left(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        ..Default::default()
    };
    let init_value = connection.initialize(serde_json::to_value(capabilities)?)?;
    let init: InitializeParams = serde_json::from_value(init_value)?;

    #[allow(deprecated)]
    let root = init.root_uri.as_ref().and_then(|u| u.to_file_path().ok());
    let mut state = ServerState::new(root);
    if let Some(options) = &init.initialization_options {
        state.config.update(options);
    }

    main_loop(&connection, &mut state)
}

fn main_loop(
    connection: &Connection,
    state: &mut ServerState,
) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                let resp = handle_request(state, req);
                connection.sender.send(Message::Response(resp))?;
            }
            Message::Notification(notif) => {
                handle_notification(connection, state, notif)?;
            }
            Message::Response(_) => {}
        }
    }
    Ok(())
}

fn handle_request(state: &mut ServerState, req: Request) -> Response {
    match req.method.as_str() {
        GotoDefinition::METHOD => typed_request::<GotoDefinition, _>(req, |params| {
            let uri = &params.text_document_position_params.text_document.uri;
            let position = params.text_document_position_params.position;
            let path = url_to_path(uri)?;
            let snap = build_snapshot(state, &path);
            features::definition(&snap, &path, position).map(GotoDefinitionResponse::Scalar)
        }),
        HoverRequest::METHOD => typed_request::<HoverRequest, _>(req, |params| {
            let uri = &params.text_document_position_params.text_document.uri;
            let position = params.text_document_position_params.position;
            let path = url_to_path(uri)?;
            let snap = build_snapshot(state, &path);
            features::hover(&snap, &path, position)
        }),
        DocumentSymbolRequest::METHOD => typed_request::<DocumentSymbolRequest, _>(req, |params| {
            let path = url_to_path(&params.text_document.uri)?;
            let snap = build_snapshot(state, &path);
            Some(DocumentSymbolResponse::Nested(features::document_symbols(
                &snap, &path,
            )))
        }),
        _ => Response::new_err(
            req.id,
            lsp_server::ErrorCode::MethodNotFound as i32,
            format!("unhandled method: {}", req.method),
        ),
    }
}

/// リクエストのデシリアライズと結果のシリアライズを共通化する
fn typed_request<R, F>(req: Request, f: F) -> Response
where
    R: lsp_types::request::Request,
    F: FnOnce(R::Params) -> R::Result,
{
    let id: RequestId = req.id.clone();
    match serde_json::from_value::<R::Params>(req.params) {
        Ok(params) => {
            let result = f(params);
            Response::new_ok(id, serde_json::to_value(result).unwrap_or_default())
        }
        Err(e) => Response::new_err(
            id,
            lsp_server::ErrorCode::InvalidParams as i32,
            e.to_string(),
        ),
    }
}

fn handle_notification(
    connection: &Connection,
    state: &mut ServerState,
    notif: Notification,
) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
    match notif.method.as_str() {
        DidOpenTextDocument::METHOD => {
            let params: lsp_types::DidOpenTextDocumentParams =
                serde_json::from_value(notif.params)?;
            if let Some(path) = url_to_path(&params.text_document.uri) {
                state.docs.insert(path.clone(), params.text_document.text);
                publish_diagnostics(connection, state, &path)?;
            }
        }
        DidChangeTextDocument::METHOD => {
            let params: lsp_types::DidChangeTextDocumentParams =
                serde_json::from_value(notif.params)?;
            if let Some(path) = url_to_path(&params.text_document.uri) {
                // FULL 同期: 最後の変更が全文
                if let Some(change) = params.content_changes.into_iter().last() {
                    state.docs.insert(path.clone(), change.text);
                }
                publish_diagnostics(connection, state, &path)?;
            }
        }
        DidSaveTextDocument::METHOD => {}
        DidChangeConfiguration::METHOD => {
            let params: lsp_types::DidChangeConfigurationParams =
                serde_json::from_value(notif.params)?;
            // クライアント設定 ({"tasm": {...}} または直接) の両形式を受ける
            let settings = params
                .settings
                .get("tasm")
                .cloned()
                .unwrap_or(params.settings);
            state.config.update(&settings);
            let open_docs: Vec<PathBuf> = state.docs.keys().cloned().collect();
            for path in open_docs {
                publish_diagnostics(connection, state, &path)?;
            }
        }
        _ => {}
    }
    Ok(())
}

/// 対象ファイルのユニットを解析し、ファイル別に診断を publish する。
/// 前回 publish して今回消えたファイルには空の診断を送ってクリアする。
fn publish_diagnostics(
    connection: &Connection,
    state: &mut ServerState,
    target: &Path,
) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
    let snap = build_snapshot(state, target);
    let by_file = diagnostics(&snap, target);

    let mut current: std::collections::HashSet<PathBuf> = by_file.keys().cloned().collect();
    for path in state.published.iter() {
        if !by_file.contains_key(path) {
            send_diagnostics(connection, path, vec![])?;
        }
    }
    for (path, diags) in by_file {
        send_diagnostics(connection, &path, diags)?;
    }
    std::mem::swap(&mut state.published, &mut current);
    Ok(())
}

fn send_diagnostics(
    connection: &Connection,
    path: &Path,
    diagnostics: Vec<lsp_types::Diagnostic>,
) -> Result<(), Box<dyn std::error::Error + Sync + Send>> {
    let Some(uri): Option<Url> = path_to_url(path) else {
        return Ok(());
    };
    let params = PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    };
    connection
        .sender
        .send(Message::Notification(Notification::new(
            PublishDiagnostics::METHOD.to_string(),
            params,
        )))?;
    Ok(())
}
