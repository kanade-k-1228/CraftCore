# コンパイラアーキテクチャ

## 1. Grammer

コンパイラは最初にソースコードファイルからASTを構築します。ここからコード生成の旅が始まります。

## 2. collect

AST からグローバルオブジェクトを収集します。
これらの収集は Const -> Type -> Static -> Asm / Fn の順番でなされます。
この依存関係が循環しないように、Const は Const だけで完結し、Type は Const のみに、Static は Const と Type のみに依存するように言語が設計されています。

### 2-1. const

AST から const を収集し `HashMap<string, (ast, addr, ResolvedType, size, value)>` にまとめます。
初期値の constexpr を評価し、型とメモリサイズを推論します。

### 2-2. type

AST から type を収集し `HashMap<string, (ast, ResolvedType, size)>` にまとめます。
型に含まれる const を評価し、基本型のみで書かれた ResolvedType を求め、型のサイズを確定します。

### 2-3. static

AST から static を収集し `HashMap<string, (ast, addr, ResolvedType, size)>` にまとめます。
型とサイズを確定します。

### 2-4. asm

AST から asm を収集し `HashMap<string, (ast, addr, vec<code>)>` にまとめます。

### 2-5. fn

AST から fn を収集し `HashMap<string, (ast, addr, deps)>` にまとめます。
fn が依存するグローバルオブジェクトの一覧もまとめます。

## 3. convert

各関数をアセンブリシーケンスに変換します。このアセンブリは不完全で、未決定のオペランドを含みます。
関数内の変数をレジスタ・スタックに配置しながら、各式をアセンブリに変換します。

## 4. link

グローバルオブジェクトを配置します。定義 const / static / asm / fn をプログラムメモリ空間およびデータメモリ空間に配置し、各グローバルオブジェクトのアドレスを決定します。Collectした時点では部分的にNoneだったaddrを確定します。

## 5. bingen

３で生成したアセンブリのうち、未確定だったオペランド（グローバルオブジェクトのアドレス）を埋めてバイナリを生成します。
