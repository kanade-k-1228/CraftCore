# TASM: Typed assembly language

自作マイコン用のアセンブリ言語 `tasm` (typed assembly) です。

- 組み込みシステムを作るために実用上十分な機能を持つ
- コンパイラの実装が簡単になる
- 文法がわかりやすい

## サンプルコード

```tasm
type Vec3 = {x: int, y: int, z: int};

const p: [3]Vec3 = [
  {x: 1, y: 2, z: 3},
  {x: 4, y: 5, z: 6},
  {x: 7, y: 8, z: 9},
];

fn add(a: Vec3, b: Vec3) -> Vec3 {
  return {x: a.x + b.x, y: a.y + b.y};
}

fn main() {
  var sum = add(add(p[0], p[1]), p[2]);
  return {x: sum.x/3, y: sum.y/3, z: sum.z/3};
}

static pwcsr @ 0x0010 : int; // Power Controll Registor

fn halt() {
  pwcsr = 0x0001;
}

asm reset @ 0x0000 {
  call(main);
  call(halt);
}
```

コンパイラからの出力はディレクトリout以下にまとめられます。
コンパイラからは次の２種類のバイナリファイルが出力されます：
 - `*.p.bin` : プログラムに配置される
 - `*.c.bin` : データメモリ空間の Const 領域 (EEPROM) に配置される値
またデバッグ情報として次の２ファイルが出力されます：
 - `*.f.map` : プログラム領域の関数名とアドレスの対応
 - `*.s.map` : 関数フレームのスタックの変数名とアドレスの対応
 - `*.d.map` : データメモリ空間の Static/Const の変数名とアドレスの対応
