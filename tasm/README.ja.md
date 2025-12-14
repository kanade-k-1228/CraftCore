# TASM: Typed assembly language

自作マイコン用の自作言語 `tasm` (typed assembly) です。

低級言語の「生成されるバイナリが想像しやすい」という特徴と、高級言語の「型などの抽象化機能が豊富であること」を両立するために設計しました。

- 組み込みシステムを作るために実用上十分な機能を持つ
- コンパイラの実装が簡単になる
- 文法がわかりやすいこと

## サンプルコード

```tasm
type Vec3 = {x: int, y: int, z: int};

fn add(a: Vec3, b: Vec3) -> Vec3 {
  return {x: a.x + b.x, y: a.y + b.y};
}

fn calc() {
  var p: [3]Vec3 = [
    {x: 1, y: 2, z: 3},
    {x: 4, y: 5, z: 6},
    {x: 7, y: 8, z: 9},
  ];
  var sum = add(add(p[0],p[1]),p[2]);
  return {x: sum.x/3, y: sum.y/3, z: sum.z/3};
}

asm main() {
  calc();
}
```

最終的なエントリポイントは asm main になります。
生成されたコードは main がそのまま出てきます。mainの前に挿入される処理はありません。
