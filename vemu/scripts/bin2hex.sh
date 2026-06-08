#!/usr/bin/env bash
# bin2hex.sh — raw バイナリを $readmemh 用 hex テキストに変換する
#
# 使い方:
#   bin2hex.sh <input.bin> <output.hex> [width]
#     width: 1 (8bit, デフォルト) | 2 (16bit) | 4 (32bit)
#
# $readmemh はファイル内の各値を mem[index] に順に格納する。
# 配列要素のビット幅 = width*8 と一致させること。
# 16/32bit はリトルエンディアン読み出し (tasm 出力に合わせる)。

set -euo pipefail

if [[ $# -lt 2 ]]; then
    echo "usage: $0 <input.bin> <output.hex> [width=1|2|4]" >&2
    exit 1
fi

IN="$1"
OUT="$2"
WIDTH="${3:-1}"

case "$WIDTH" in
    1) od -An -v -tx1 -w1 "$IN" | sed 's/^ *//' > "$OUT" ;;
    2) od -An -v -tx2 -w2 --endian=little "$IN" | sed 's/^ *//' > "$OUT" ;;
    4) od -An -v -tx4 -w4 --endian=little "$IN" | sed 's/^ *//' > "$OUT" ;;
    *) echo "error: width must be 1, 2, or 4" >&2; exit 1 ;;
esac

echo "wrote $(wc -l < "$OUT") lines to $OUT" >&2
