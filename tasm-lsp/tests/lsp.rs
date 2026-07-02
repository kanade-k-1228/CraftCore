//! Connection::memory() によるインプロセス統合テスト。
//! サーバは別スレッドで tasm_lsp::run を実行し、クライアント側から
//! JSON-RPC メッセージを直接やり取りする。

use std::path::{Path, PathBuf};
use std::time::Duration;

use lsp_server::{Connection, Message, Notification, Request, RequestId};
use serde_json::{json, Value};

const TIMEOUT: Duration = Duration::from_secs(10);

struct TestClient {
    conn: Connection,
    next_id: i32,
    server: Option<std::thread::JoinHandle<()>>,
    /// 受信済み publishDiagnostics (uri → diagnostics)
    diagnostics: Vec<(String, Value)>,
}

impl TestClient {
    /// fixture ルートと includeDirs を指定してサーバを起動し、ハンドシェイクまで済ませる
    fn start(root: &Path, include_dirs: &[&str]) -> Self {
        let (server_conn, client_conn) = Connection::memory();
        let server = std::thread::spawn(move || {
            tasm_lsp::run(server_conn).unwrap();
        });
        let mut client = TestClient {
            conn: client_conn,
            next_id: 0,
            server: Some(server),
            diagnostics: vec![],
        };
        let root_uri = format!("file://{}", root.display());
        client.request(
            "initialize",
            json!({
                "capabilities": {},
                "rootUri": root_uri,
                "initializationOptions": {"includeDirs": include_dirs},
            }),
        );
        client.notify("initialized", json!({}));
        client
    }

    fn notify(&self, method: &str, params: Value) {
        self.conn
            .sender
            .send(Message::Notification(Notification {
                method: method.to_string(),
                params,
            }))
            .unwrap();
    }

    /// リクエストを送り、レスポンスが来るまでの publishDiagnostics を溜め込む
    fn request(&mut self, method: &str, params: Value) -> Value {
        self.next_id += 1;
        let id = RequestId::from(self.next_id);
        self.conn
            .sender
            .send(Message::Request(Request {
                id: id.clone(),
                method: method.to_string(),
                params,
            }))
            .unwrap();
        loop {
            match self.conn.receiver.recv_timeout(TIMEOUT).unwrap() {
                Message::Response(resp) if resp.id == id => {
                    assert!(resp.error.is_none(), "server error: {:?}", resp.error);
                    return resp.result.unwrap_or(Value::Null);
                }
                Message::Notification(n) if n.method == "textDocument/publishDiagnostics" => {
                    let uri = n.params["uri"].as_str().unwrap().to_string();
                    self.diagnostics
                        .push((uri, n.params["diagnostics"].clone()));
                }
                _ => {}
            }
        }
    }

    fn open(&mut self, path: &Path, text: &str) {
        self.notify(
            "textDocument/didOpen",
            json!({
                "textDocument": {
                    "uri": uri_of(path),
                    "languageId": "tasm",
                    "version": 1,
                    "text": text,
                }
            }),
        );
        // documentSymbol をバリアに使い didOpen 由来の診断をすべて受信する
        self.request(
            "textDocument/documentSymbol",
            json!({"textDocument": {"uri": uri_of(path)}}),
        );
    }

    /// path 宛の最後の publishDiagnostics
    fn diags_for(&self, path: &Path) -> Option<&Value> {
        let uri = uri_of(path);
        self.diagnostics
            .iter()
            .rev()
            .find(|(u, _)| *u == uri)
            .map(|(_, d)| d)
    }

    fn shutdown(mut self) {
        self.request("shutdown", Value::Null);
        self.notify("exit", Value::Null);
        self.server.take().unwrap().join().unwrap();
    }
}

fn uri_of(path: &Path) -> String {
    format!("file://{}", path.display())
}

/// テストごとに一意な fixture ディレクトリを作る
fn fixture(name: &str, files: &[(&str, &str)]) -> PathBuf {
    let root = std::env::temp_dir().join(format!("tasm-lsp-test-{}-{}", std::process::id(), name));
    let _ = std::fs::remove_dir_all(&root);
    for (rel, text) in files {
        let path = root.join(rel);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(&path, text).unwrap();
    }
    root.canonicalize().unwrap()
}

const UTIL_TASM: &str = "fn add(a: int, b: int) -> int {\n    return a + b;\n}\n";
const MAIN_TASM: &str =
    "fn main() -> int {\n    var x: int = mylib::util::add(1, 2);\n    return x;\n}\n";

#[test]
fn parse_error_diagnostics() {
    let root = fixture("parse-err", &[("main.tasm", "fn broken( {\n")]);
    let main = root.join("main.tasm");
    let mut client = TestClient::start(&root, &[]);
    client.open(&main, "fn broken( {\n");

    let diags = client.diags_for(&main).expect("diagnostics published");
    let arr = diags.as_array().unwrap();
    assert!(!arr.is_empty(), "expected parse error, got {diags}");
    // `{` (col 12, 0-indexed 11) が UnexpectedToken
    assert_eq!(arr[0]["range"]["start"]["line"], 0);
    assert_eq!(arr[0]["range"]["start"]["character"], 11);
    client.shutdown();
}

#[test]
fn multifile_unit_no_diagnostics() {
    let root = fixture(
        "multifile-ok",
        &[("main.tasm", MAIN_TASM), ("mylib/util.tasm", UTIL_TASM)],
    );
    let main = root.join("main.tasm");
    let mut client = TestClient::start(&root, &["mylib"]);
    client.open(&main, MAIN_TASM);

    if let Some(diags) = client.diags_for(&main) {
        assert_eq!(diags.as_array().unwrap().len(), 0, "unexpected: {diags}");
    }
    client.shutdown();
}

#[test]
fn semantic_error_diagnostics() {
    let text = "fn main() -> int {\n    return undefined_symbol(1);\n}\n";
    let root = fixture("semantic-err", &[("main.tasm", text)]);
    let main = root.join("main.tasm");
    let mut client = TestClient::start(&root, &[]);
    client.open(&main, text);

    let diags = client.diags_for(&main).expect("diagnostics published");
    let arr = diags.as_array().unwrap();
    assert!(!arr.is_empty(), "expected semantic error");
    let msg = arr[0]["message"].as_str().unwrap();
    assert!(msg.contains("undefined_symbol"), "message: {msg}");
    // undefined_symbol は line 1 col 11 (0-indexed)
    assert_eq!(arr[0]["range"]["start"]["line"], 1);
    assert_eq!(arr[0]["range"]["start"]["character"], 11);
    client.shutdown();
}

#[test]
fn diagnostics_cleared_after_fix() {
    let broken = "fn main() -> int {\n    return undefined_symbol(1);\n}\n";
    let root = fixture("clear-diags", &[("main.tasm", broken)]);
    let main = root.join("main.tasm");
    let mut client = TestClient::start(&root, &[]);
    client.open(&main, broken);
    assert!(!client
        .diags_for(&main)
        .unwrap()
        .as_array()
        .unwrap()
        .is_empty());

    let fixed = "fn main() -> int {\n    return 1;\n}\n";
    client.notify(
        "textDocument/didChange",
        json!({
            "textDocument": {"uri": uri_of(&main), "version": 2},
            "contentChanges": [{"text": fixed}],
        }),
    );
    client.request(
        "textDocument/documentSymbol",
        json!({"textDocument": {"uri": uri_of(&main)}}),
    );
    let diags = client.diags_for(&main).unwrap();
    assert_eq!(diags.as_array().unwrap().len(), 0, "should be cleared");
    client.shutdown();
}

#[test]
fn definition_across_modules() {
    let root = fixture(
        "definition",
        &[("main.tasm", MAIN_TASM), ("mylib/util.tasm", UTIL_TASM)],
    );
    let main = root.join("main.tasm");
    let util = root.join("mylib/util.tasm");
    let mut client = TestClient::start(&root, &["mylib"]);
    client.open(&main, MAIN_TASM);

    // "mylib::util::add" の add (line 1, col 30-32 0-indexed) で定義ジャンプ
    let resp = client.request(
        "textDocument/definition",
        json!({
            "textDocument": {"uri": uri_of(&main)},
            "position": {"line": 1, "character": 30},
        }),
    );
    assert_eq!(resp["uri"].as_str().unwrap(), uri_of(&util));
    assert_eq!(resp["range"]["start"]["line"], 0);
    assert_eq!(resp["range"]["start"]["character"], 3);
    assert_eq!(resp["range"]["end"]["character"], 6);

    // 先頭セグメント (mylib) 上でも同じ定義に飛ぶ
    let resp = client.request(
        "textDocument/definition",
        json!({
            "textDocument": {"uri": uri_of(&main)},
            "position": {"line": 1, "character": 18},
        }),
    );
    assert_eq!(resp["uri"].as_str().unwrap(), uri_of(&util));
    client.shutdown();
}

#[test]
fn definition_local_var_and_param() {
    let root = fixture(
        "definition-local",
        &[("main.tasm", MAIN_TASM), ("mylib/util.tasm", UTIL_TASM)],
    );
    let main = root.join("main.tasm");
    let util = root.join("mylib/util.tasm");
    let mut client = TestClient::start(&root, &["mylib"]);
    client.open(&main, MAIN_TASM);

    // "return x;" の x → var x の宣言位置 (line 1 col 8)
    let resp = client.request(
        "textDocument/definition",
        json!({
            "textDocument": {"uri": uri_of(&main)},
            "position": {"line": 2, "character": 11},
        }),
    );
    assert_eq!(resp["uri"].as_str().unwrap(), uri_of(&main));
    assert_eq!(resp["range"]["start"]["line"], 1);
    assert_eq!(resp["range"]["start"]["character"], 8);

    // util.tasm 内 "return a + b;" の a → パラメータ位置
    client.open(&util, UTIL_TASM);
    let resp = client.request(
        "textDocument/definition",
        json!({
            "textDocument": {"uri": uri_of(&util)},
            "position": {"line": 1, "character": 11},
        }),
    );
    assert_eq!(resp["uri"].as_str().unwrap(), uri_of(&util));
    assert_eq!(resp["range"]["start"]["line"], 0);
    assert_eq!(resp["range"]["start"]["character"], 7);
    client.shutdown();
}

#[test]
fn hover_function_signature() {
    let root = fixture(
        "hover",
        &[("main.tasm", MAIN_TASM), ("mylib/util.tasm", UTIL_TASM)],
    );
    let main = root.join("main.tasm");
    let mut client = TestClient::start(&root, &["mylib"]);
    client.open(&main, MAIN_TASM);

    let resp = client.request(
        "textDocument/hover",
        json!({
            "textDocument": {"uri": uri_of(&main)},
            "position": {"line": 1, "character": 30},
        }),
    );
    let value = resp["contents"]["value"].as_str().unwrap();
    assert!(
        value.contains("fn add(a: int, b: int) -> int"),
        "hover: {value}"
    );
    assert!(value.contains("mylib::util"), "hover: {value}");
    client.shutdown();
}

/// 実リポジトリの example が project.yaml 探索だけで (設定なしで) 解析できること
#[test]
fn real_repo_example_is_clean_via_project_yaml() {
    let repo = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf();
    let main = repo.join("example/06_task_switch/main.tasm");
    if !repo.join("rtos").exists() || !main.exists() {
        return;
    }
    let state = tasm_lsp::state::ServerState::new(Some(repo.clone()));
    let snap = tasm_lsp::analysis::build_snapshot(&state, &main);
    assert!(snap.parse_errors.is_empty(), "{:?}", snap.parse_errors);
    assert!(snap.files.len() > 1, "rtos module files should be included");
    let diags = tasm_lsp::analysis::diagnostics(&snap, &main);
    assert!(diags.is_empty(), "{:?}", diags);
}

const PROJECT_YAML: &str = "include:\n  mylib: mylib\n";

/// project.yaml の includes (+ srcs デフォルト main.tasm) でユニットが組めること
#[test]
fn project_yaml_discovery() {
    let root = fixture(
        "project-yaml",
        &[
            ("project.yaml", PROJECT_YAML),
            ("main.tasm", MAIN_TASM),
            ("mylib/util.tasm", UTIL_TASM),
        ],
    );
    let main = root.join("main.tasm");
    let util = root.join("mylib/util.tasm");
    // includeDirs 設定なしで起動 → project.yaml から解決される
    let mut client = TestClient::start(&root, &[]);
    client.open(&main, MAIN_TASM);

    if let Some(diags) = client.diags_for(&main) {
        assert_eq!(diags.as_array().unwrap().len(), 0, "unexpected: {diags}");
    }
    let resp = client.request(
        "textDocument/definition",
        json!({
            "textDocument": {"uri": uri_of(&main)},
            "position": {"line": 1, "character": 30},
        }),
    );
    assert_eq!(resp["uri"].as_str().unwrap(), uri_of(&util));
    client.shutdown();
}

/// includes のキーがモジュール名になること (ディレクトリ名とは無関係)
#[test]
fn project_yaml_module_name_from_key() {
    let root = fixture(
        "project-yaml-key",
        &[
            ("project.yaml", "include:\n  mylib: libsrc\n"),
            ("main.tasm", MAIN_TASM),
            ("libsrc/util.tasm", UTIL_TASM),
        ],
    );
    let main = root.join("main.tasm");
    let util = root.join("libsrc/util.tasm");
    let mut client = TestClient::start(&root, &[]);
    client.open(&main, MAIN_TASM);

    // mylib::util::add がディレクトリ libsrc/ の util.tasm に解決される
    if let Some(diags) = client.diags_for(&main) {
        assert_eq!(diags.as_array().unwrap().len(), 0, "unexpected: {diags}");
    }
    let resp = client.request(
        "textDocument/definition",
        json!({
            "textDocument": {"uri": uri_of(&main)},
            "position": {"line": 1, "character": 30},
        }),
    );
    assert_eq!(resp["uri"].as_str().unwrap(), uri_of(&util));
    client.shutdown();
}

/// エディタ設定 includeDirs の `name=dir` 形式でも同様に解決されること
#[test]
fn settings_include_name_from_spec() {
    let root = fixture(
        "settings-key",
        &[("main.tasm", MAIN_TASM), ("libsrc/util.tasm", UTIL_TASM)],
    );
    let main = root.join("main.tasm");
    let util = root.join("libsrc/util.tasm");
    let mut client = TestClient::start(&root, &["mylib=libsrc"]);
    client.open(&main, MAIN_TASM);

    if let Some(diags) = client.diags_for(&main) {
        assert_eq!(diags.as_array().unwrap().len(), 0, "unexpected: {diags}");
    }
    let resp = client.request(
        "textDocument/definition",
        json!({
            "textDocument": {"uri": uri_of(&main)},
            "position": {"line": 1, "character": 30},
        }),
    );
    assert_eq!(resp["uri"].as_str().unwrap(), uri_of(&util));
    client.shutdown();
}

/// srcs に複数ファイルを列挙したプロジェクト: ルート間のシンボル参照が解決できること
#[test]
fn project_yaml_multiple_srcs() {
    let a = "fn main() -> int {\n    return helper();\n}\n";
    let b = "fn helper() -> int {\n    return 7;\n}\n";
    let root = fixture(
        "project-yaml-srcs",
        &[
            ("project.yaml", "src:\n  - a.tasm\n  - b.tasm\n"),
            ("a.tasm", a),
            ("b.tasm", b),
        ],
    );
    let a_path = root.join("a.tasm");
    let b_path = root.join("b.tasm");
    let mut client = TestClient::start(&root, &[]);
    client.open(&a_path, a);

    if let Some(diags) = client.diags_for(&a_path) {
        assert_eq!(diags.as_array().unwrap().len(), 0, "unexpected: {diags}");
    }
    let resp = client.request(
        "textDocument/definition",
        json!({
            "textDocument": {"uri": uri_of(&a_path)},
            "position": {"line": 1, "character": 12},
        }),
    );
    assert_eq!(resp["uri"].as_str().unwrap(), uri_of(&b_path));
    assert_eq!(resp["range"]["start"]["line"], 0);
    assert_eq!(resp["range"]["start"]["character"], 3);
    client.shutdown();
}

/// モジュールファイルを直接開いても project.yaml の srcs 込みで解析されること
#[test]
fn project_yaml_module_file_open() {
    let root = fixture(
        "project-yaml-module",
        &[
            ("project.yaml", PROJECT_YAML),
            ("main.tasm", MAIN_TASM),
            ("mylib/util.tasm", UTIL_TASM),
        ],
    );
    let util = root.join("mylib/util.tasm");
    let mut client = TestClient::start(&root, &[]);
    client.open(&util, UTIL_TASM);

    if let Some(diags) = client.diags_for(&util) {
        assert_eq!(diags.as_array().unwrap().len(), 0, "unexpected: {diags}");
    }
    client.shutdown();
}

#[test]
fn document_symbols_lists_defs() {
    let text = "const N = 42;\nstatic counter: int;\nfn main() -> int {\n    return N;\n}\n";
    let root = fixture("symbols", &[("main.tasm", text)]);
    let main = root.join("main.tasm");
    let mut client = TestClient::start(&root, &[]);
    client.notify(
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": uri_of(&main),
                "languageId": "tasm",
                "version": 1,
                "text": text,
            }
        }),
    );
    let resp = client.request(
        "textDocument/documentSymbol",
        json!({"textDocument": {"uri": uri_of(&main)}}),
    );
    let symbols = resp.as_array().unwrap();
    let names: Vec<&str> = symbols
        .iter()
        .map(|s| s["name"].as_str().unwrap())
        .collect();
    assert_eq!(names, vec!["N", "counter", "main"]);
    assert_eq!(symbols[0]["kind"], 14); // CONSTANT
    assert_eq!(symbols[1]["kind"], 13); // VARIABLE
    assert_eq!(symbols[2]["kind"], 12); // FUNCTION
    client.shutdown();
}
