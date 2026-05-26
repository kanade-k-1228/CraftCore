# コンパイラアーキテクチャ

`tasm` コンパイラは入力のソースファイル群を読み、命令メモリ (i.bin) / 定数メモリ (rom.bin) / シンボルマップ (map.yaml) を出力します。

```
[*.tasm] ──▶
1. Lexer
2. Parser
3. AST
4. Global
    ├── normtype
    ├── constexpr
    ├── typeinfer
    ├── deps
    ├── func2code
    └── asm2code
5. Allocator
    ├──▶ i.bin
    ├──▶ rom.bin
    └──▶ map.yaml
```

主なステージは以下の通りです。

## 1. Lex (`grammer::lexer`)

ソース文字列を 1 行ずつトークン列に変換します。

- 単一文字トークン / 2 文字トークン (`==`, `<=`, `->` など) / キーワード (`fn`, `asm`, `if`, `as`, `sizeof` …) を識別
- 数値リテラル (`0x...` の 16 進、10 進、`_` 区切り)、文字リテラル (`'A'`, `'\n'`)、文字列リテラル (`"..."`) をパース
- `//` 以降の行末コメントをスキップ
- 位置情報 (`Pos`: ファイル名・行・列) を各トークンに付与

## 2. Parse (`grammer::parser`)

トークン列を AST に変換します。AST は `Vec<Def>` を持つ `AST` 構造体で、各 `Def` は以下のいずれかです。

| Def    | 内容                                             |
| ------ | ------------------------------------------------ |
| Type   | `type ident = type;`                             |
| Const  | `const [@ expr] ident = expr;` (初期値式が必須)  |
| Static | `static [@ expr] ident : type;` (アドレス指定可) |
| Asm    | `asm [@ expr] ident { asm_stmt* }`               |
| Func   | `fn ident(args) [-> type] { stmt* }`             |

`Stmt` / `Expr` / `Type` は再帰的な enum として定義されます。式は優先順位ごとに再帰下降でパースされます (`or → xor → and → eq → relat → shift → add → mul → unary → postfix → prim`)。

パースに失敗した場合はその場でエラーを記録し、次の定義キーワード (`type` / `const` / `static` / `asm` / `fn`) まで読み飛ばすことで複数エラーをまとめて報告します。

## 3. Global (`eval::global`)

AST 内の各定義を名前で引ける `IndexMap<&str, &Def>` にまとめた、評価のためのデータベースです。重複定義はこの段階で検出されます。

`Global` は以下の評価結果を **遅延・キャッシュ付き** で提供します (`RwLock<HashMap<...>>`)。

| API               | 役割                                                              |
| ----------------- | ----------------------------------------------------------------- |
| `normtype(ty)`    | AST の型を `NormType` (基本型のみで書かれた正規化型) に変換       |
| `constexpr(expr)` | 定数式を `ConstExpr` (数値・文字・文字列・配列・構造体) に評価    |
| `typeinfer(expr)` | 式の型を `NormType` として推論                                    |
| `addrexpr(expr)`  | `ident[i].field` のようなアドレス表現を `(symbol, offset)` に解決 |
| `code(name)`      | `asm` / `fn` 定義からコード (`Code = Vec<Inst<Reg, Imm>>`) を生成 |

`get_*_resolved(name)` 系は `@ expr` で指定された固定アドレスや配置サイズなど、リンク前に確定する情報をまとめて取り出すヘルパです。

### NormType

`NormType` は AST 上の `Type::Custom(ident)` を解決し、配列長を `usize` に確定させた正規化形です。`sizeof()` はこの型に対して機械的に計算されます。

### ConstExpr

`ConstExpr` は定数式の評価結果です。`Number` / `Char` / `String` / `Array` / `Struct` を保持し、定数定義の初期値・配列長・固定アドレス指定など複数の文脈で再利用されます。

## 4. Code 生成

`Global::code(name)` は内部で次のいずれかを呼びます。

### 4.1. `asm2code` (`eval::asm`)

`asm` ブロック内の各文を `arch::inst::Inst<Reg, Imm>` に変換します。

- 引数 `Expr` をレジスタ名 (`reg`) / 即値 (`imm`) / グローバルラベル (`global`) / ローカルラベル (`local`) として解釈
- `Imm` は未解決の即値で、`Lit` (リテラル) / `Const` (named constant) / `Symbol` (データシンボル + offset) / `Label` (コードラベル) の 4 種類を持つ
- ローカルラベルは同一 `asm` ブロック内の相対オフセットとして即時解決される

### 4.2. `func2code` (`eval::func`)

`fn` 定義をスタックフレーム付きのアセンブリ列に変換します。

- `Local` (`eval::local`) が引数とローカル変数をスタックオフセットに割り付ける (引数は FP+正、`var` は FP-負)
- プロローグでスタックを確保し、`RA` / `FP` の保存と新 `FP` 設定、引数 (A0, A1, …) のスタック保存を行う
- 文ごとに `compile_stmt` / `compile_expr` / `compile_lvalue` を再帰的に呼び、`if`/`while` は前方/後方の相対ジャンプ (`JUMPIFR` / `JUMPR`) に展開される
- エピローグで `FP` を復元し、`RET` で呼び出し元に戻る

呼び出し規約: 第 1〜2 引数は `A0` / `A1`、それ以上はスタック渡し。戻り値は `A0`。

## 5. Dependency Resolution (`eval::deps`)

`Global::deps(entries, ...)` がエントリーポイント (デフォルトでは `reset`, `irq`, `main`) から到達可能なコードラベルとデータシンボルを再帰的に列挙します。

- `Imm::Label(name)` を辿って到達可能な `asm` / `fn` を `labels` に追加
- `Imm::Symbol(name, _)` をデータシンボルとして `symbols` に追加
- 未使用のグローバル定義はリンク対象から除外される

## 6. Allocator (`linker::allocator`, `linker::memory`)

`Memory::new(begin, end).section(name, start, end)...` でアドレス空間とセクション範囲を宣言し、`Allocator` がオブジェクトを配置します。

### 6.1. 命令メモリ

```
0x0000..0x0004  reset    section
0x0004..0x0008  irq      section
0x0008..0x10000 code     section
```

- 固定アドレス指定 (`asm @ ...`) のオブジェクトは `allocate(addr, size, name)` で配置
- アドレス未指定のオブジェクトは `section("code", size, name)` で空き領域に詰める

### 6.2. データメモリ

```
0x3000..0x5000   const    section
0x5000..0x10000  static   section
```

- `const` / `static` の固定アドレス指定 (`@ ...`) は `allocate` で配置
- 未指定の `const` は `const` セクション、未指定の `static` は `static` セクションへ詰められる

`Allocator` は内部的に `[begin, end)` の `Segment` 列を持ち、`allocate` 時には対応する `Segment` を 3 つに分割 (前空き / 占有 / 後空き) することで領域を確保します。重複や領域外への配置はエラーになります。

## 7. Binary Generation (`linker::binary`)

確定した `imap` / `dmap` (シンボル → アドレス) を使って未解決の `Imm` を実アドレスに置換し、バイナリを書き出します。

- `genibin` — 各命令の `Imm` を解決 (`Label` → コードアドレス、`Symbol` → データアドレス + offset、`Const` → 値、`Lit` → そのまま) し、`Inst::to_op().to_bin()` で 32bit 命令に変換、リトルエンディアンで命令メモリイメージに書き込む
- `gencbin` — `const` 定義の `ConstExpr::bin()` を該当アドレスに配置してデータ ROM イメージを生成

## 8. Symbol Map (`util::maps`)

`SymbolMap::generate` がコードとデータの配置結果を YAML に書き出します (`map.yaml`)。

```
code:
  main:
    addr: 0x...
    size: 0x...
    stacks: { ... }
data:
  serial_tx:
    addr: 0x...
    size: 0x...
```

デバッガ・モニタとの突き合わせに利用します。

## main.rs の流れ

1. `*.tasm` を読み込み、`Lexer` でトークン化
2. 全トークンを 1 つに結合して `Parser` で AST に
3. `Global::new(ast)` を構築
4. `deps(&["reset", "irq", "main"])` で到達可能シンボルを収集
5. 命令メモリの `Allocator` を作り、固定アドレス → 自動配置の順に `asm` / `fn` を割り当て
6. データメモリの `Allocator` を作り、固定アドレス → 自動配置の順に `const` / `static` を割り当て
7. `genibin` / `gencbin` でバイナリを生成
8. 出力ファイル (`-o` / `-r` / `-m`) を書き出し
