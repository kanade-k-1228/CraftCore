# CraftCore: Learn the fundamentals of computers

CraftCore is a project to build a CPU with standard logic IC's, and run custom OS on it.

- **Simple**: We keep it simple instead of going for optimization, so it's easy to implement.
- **Sufficient**: It has sufficient functionality to run various programs.

## Archtecture: RK16

RK16 is a simple RISC ISA.

## Assembler: rkasm

## Emulator: rkemu

## RKOS: Real Time OS for RK16

## Tutorial

### Install

```
$ cargo install --path asm
$ cargo install --path emu
```

### Assemble

```
$ rkasm -d sample/00_asm_test/main.rk
RK16 Assembler by kanade-k-1228
1. Read Files and Parse Lines
  < sample/00_asm_test/main.rk
2. Resolve Label & Generate Binary
  > main.rk.bin
-------------------+------[sample/00_asm_test/main.rk]-------------------
                   |    1: ; This is a sample code for rk16 assembly
                   |    2:
                   |    3: @0x0200 hoge
                   |    4: @0x0123 fuga
                   |    5: @0x4567 piyo
                   |    6: @0x89AB foo
                   |    7: @0xCDEF bar
                   |    8: @0x0010 exit
                   |    9:
                   |   10: #0x0100 const
                   |   11: #0x0030 '0'
                   |   12: #0x0031 '1'
                   |   13: #0x0032 '2'
                   |   14: #0x0033 '3'
                   |   15: #0x0034 '4'
                   |   16: #0x0035 '5'
                   |   17:
                   |   18: calc:0x0000
[0000] 00 00 98 A0 |   19:   add   T2 T0 T1     ; t2 = t0 + t1
[0001] 00 01 98 A0 |   20:   sub   T2 T0 T1     ; t2 = t0 - t1
[0002] 00 02 98 A0 |   21:   and   T2 T0 T1     ; t2 = t0 & t1
[0003] 00 03 98 A0 |   22:   or    T2 T0 T1     ; t2 = t0 | t1
[0004] 00 04 98 A0 |   23:   xor   T2 T0 T1     ; t2 = t0 ^ t1
                   |   24:
[0005] 00 05 98 A0 |   25:   eq    T2 T0 T1     ; t2 = t0 == t1
[0006] 00 06 98 A0 |   26:   neq   T2 T0 T1     ; t2 = t0 != t1
[0007] 00 07 98 A0 |   27:   lt    T2 T0 T1     ; t2 = t0 < t1
[0008] 00 08 98 A0 |   28:   lts   T2 T0 T1     ; t2 = t0 < t1
                   |   29:
[0009] 00 09 80 A0 |   30:   sr    T2 T0        ; t2 = >> t0
[000A] 00 0A 80 A0 |   31:   srs   T2 T0        ; t2 = >>> t0
[000B] 00 0B 80 A0 |   32:   srr   T2 T0        ; t2 = >>> t0
[000C] 00 0C 80 A0 |   33:   sl    T2 T0        ; t2 = <<  t0
[000D] 00 0D 80 A0 |   34:   slr   T2 T0        ; t2 = <<< t0
                   |   35:
[000E] 00 00 00 00 |   36:   nop                ; ;
[000F] 00 00 D0 C0 |   37:   mov   S0 S1        ; s0 = s1
                   |   38:
                   |   39: calci:0x0010
[0010] 01 00 80 A1 |   40:   addi  T2 T0 0x0100(const) ; t2 = t0 + const
[0011] 02 00 81 A1 |   41:   subi  T2 T0 0x0200(hoge) ; t2 = t0 - hoge
[0012] 01 23 82 A1 |   42:   andi  T2 T0 0x0123(fuga) ; t2 = t0 & fuga
[0013] 00 00 83 A1 |   43:   ori   T2 T0 0x0000 ; t2 = t0 | 0x0000
[0014] 01 10 84 A1 |   44:   xori  T2 T0 0x0110 ; t2 = t0 ^ 0x0110
                   |   45:
[0015] 11 00 85 A1 |   46:   eqi   T2 T0 0x1100 ; t2 = t0 == 0x1100
[0016] 11 00 86 A1 |   47:   neqi  T2 T0 0x1100 ; t2 = t0 != 0x1100
[0017] F0 00 87 A1 |   48:   lti   T2 T0 0xF000 ; t2 = t0 < 0xf000
[0018] F0 00 88 A1 |   49:   ltsi  T2 T0 0xF000 ; t2 = t0 < 0xf000
                   |   50:
[0019] 00 FF 84 A1 |   51:   not   T2 T0        ; t2 = ~ t0
[001A] 01 23 80 03 |   52:   loadi T0    0x0123 ; t0 = 0x0123
                   |   53:
                   |   54: memory:0x001B
[001B] 02 00 80 A3 |   55:   load  T2 T0 0x0200(hoge) ; t2 <= [t0 + hoge]
[001C] 01 23 08 A7 |   56:   store T2 T0 0x0123(fuga) ; t2 => [t0 + fuga]
                   |   57:
                   |   58: ctrl_test:0x001D
                   |   59: a:0x001D
[001D] 00 21 00 9F |   60:   if    T1    0x0021(b) ; ?t1 > b
[001E] 00 21 00 9F |   61:   ifr   T1    0x0021(b) ;
[001F] 00 24 00 0F |   62:   jump        0x0024(e) ; > e
[0020] 00 01 00 0F |   63:   jumpr       0x0001 ;
                   |   64: b:0x0021
[0021] 00 22 00 0F |   65:   call        0x0022(c) ; >>> c
                   |   66: c:0x0022
[0022] 00 00 00 4F |   67:   ret                ; <<<
                   |   68: d:0x0023
[0023] 00 00 00 1F |   69:   iret               ; !<<
                   |   70: e:0x0024
[0024] 00 24 00 0F |   71:   jump        0x0024(e)
```

Left side is the output binary, right side is the assembly code.
