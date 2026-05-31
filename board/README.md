# board

KiCad Workspace [kicad-mcp](https://github.com/lamaalrajih/kicad-mcp)

## ディレクトリ構成

```
board/
├── README.md         # 本ファイル
├── .gitignore        # kicad-mcp clone と KiCad バックアップを除外
├── .env.example      # kicad-mcp 用環境変数の雛形
├── lib/              # 自作シンボル/フットプリント
├── rk16.kicad_pro    # ← KiCad 上で初回作成 (未追加)
└── kicad-mcp/        # ← clone する MCP サーバー実体 (.gitignore 済)
```

リポジトリ直下の `.mcp.json` から `board/kicad-mcp/.venv/bin/python` を呼び出して
サーバーを起動する。MCP 設定本体は `/.mcp.json` 側にあるので注意。

## 1. kicad-mcp のセットアップ

要件:

- Python 3.10 以上
- uv 0.8.0 以上 (`pipx install uv` など)
- KiCad 9.0 以上 (実プロジェクトを開くマシン側で必要)

手順:

```bash
cd board
git clone https://github.com/lamaalrajih/kicad-mcp.git
cd kicad-mcp
make install
```

`make install` で `kicad-mcp/.venv/` が作られ、`.mcp.json` から参照しているパスと
一致する。`.mcp.json` のパスはリポジトリルートからの相対なので、CraftCore を
どこに clone しても動く。横断検索を増やしたい場合のみ `.env.example` を
コピーして編集する。

## 2. Claude Code からの利用

リポジトリのルートで Claude Code を起動すると、プロジェクトスコープの
`.mcp.json` が読み込まれ `kicad` サーバーが利用可能になる。
初回は接続許可のダイアログが出るので承認する。

接続状態の確認:

```
/mcp
```

呼び出せる主なツール: プロジェクト一覧、回路図/PCB の解析、BOM 抽出、DRC 実行など
(詳細は kicad-mcp の README を参照)。

## 3. KiCad プロジェクトの初期化

KiCad 9 上で:

1. File → New Project
2. 保存先: `board/rk16.kicad_pro`
3. Preferences → Manage Symbol/Footprint Libraries で `board/lib/` を追加

`.kicad_pro` が生成されると、`KICAD_SEARCH_PATHS` のおかげで kicad-mcp 経由で
このプロジェクトが見えるようになる。
