import { workspace, ExtensionContext } from "vscode";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | undefined;

/** 設定値中の ${workspaceFolder} を展開する (VS Code は任意設定を自動展開しない) */
function expandPath(value: string): string {
  const root = workspace.workspaceFolders?.[0]?.uri.fsPath;
  return root ? value.replace("${workspaceFolder}", root) : value;
}

export function activate(_context: ExtensionContext) {
  const config = workspace.getConfiguration("tasm");
  const serverPath = expandPath(config.get<string>("lsp.path") ?? "tasm-lsp");

  const serverOptions: ServerOptions = {
    command: serverPath,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "tasm" }],
    synchronize: {
      configurationSection: "tasm",
    },
    initializationOptions: {
      includeDirs: config.get<string[]>("includeDirs") ?? [],
    },
  };

  client = new LanguageClient(
    "tasm",
    "tasm language server",
    serverOptions,
    clientOptions
  );
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  return client?.stop();
}
