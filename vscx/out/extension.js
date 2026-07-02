"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
const vscode_1 = require("vscode");
const node_1 = require("vscode-languageclient/node");
let client;
/** 設定値中の ${workspaceFolder} を展開する (VS Code は任意設定を自動展開しない) */
function expandPath(value) {
    const root = vscode_1.workspace.workspaceFolders?.[0]?.uri.fsPath;
    return root ? value.replace("${workspaceFolder}", root) : value;
}
function activate(_context) {
    const config = vscode_1.workspace.getConfiguration("tasm");
    const serverPath = expandPath(config.get("lsp.path") ?? "tasm-lsp");
    const serverOptions = {
        command: serverPath,
    };
    const clientOptions = {
        documentSelector: [{ scheme: "file", language: "tasm" }],
        synchronize: {
            configurationSection: "tasm",
        },
        initializationOptions: {
            includeDirs: config.get("includeDirs") ?? [],
        },
    };
    client = new node_1.LanguageClient("tasm", "tasm language server", serverOptions, clientOptions);
    client.start();
}
function deactivate() {
    return client?.stop();
}
//# sourceMappingURL=extension.js.map