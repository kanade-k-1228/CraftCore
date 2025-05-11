# TASM

## typegen & tir

Resolve missing types and generate TIR (Typed Intermediate Representation) code. All expressions are fully typed, and all variables are bound to their types.

## flowgen & fir

Flowgen generates FIR (Flow Intermediate Representation) code from TIR. FIR is a higher-level representation that is easier to optimize and analyze than TIR. It is used for flow analysis, which is the process of determining how data flows through the program and how it can be optimized.
