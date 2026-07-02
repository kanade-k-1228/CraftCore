use crate::grammer::token::{Pos, Token};
use thiserror::Error;

// Unified error type for TASM
#[derive(Debug, Error)]
pub enum Error {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    // Parse errors
    #[error("{0}: Invalid token: {1:?}")]
    InvalidToken(Pos, String),

    #[error("{0}: Unexpected token: {1}")]
    UnexpectedToken(Pos, Token),

    #[error("{0}: Unexpected end of file")]
    UnexpectedEOF(Pos),

    #[error("{0}: Invalid type: {1:?}")]
    InvalidType(Pos, String),

    #[error("{0}: Invalid function: {1:?}")]
    InvalidFunction(Pos, String),

    #[error("{0}: Invalid variable: {1:?}")]
    InvalidVariable(Pos, String),

    // Link errors (no Pos - operate on symbols)
    #[error("Address conflict: {0} at 0x{1:04X} to 0x{2:04X}")]
    FixedAddressOverlapped(String, usize, usize),

    #[error("Symbol not found: {0}")]
    SymbolNotFound(String),

    #[error("Invalid section data")]
    InvalidSectionData,

    #[error("Address space overflow: Cannot allocate {1} bytes for {0}")]
    AddressSpaceOverflow(String, usize),

    #[error("Address out of range: {0} at 0x{1:04X}-0x{2:04X} is outside allowed range 0x{3:04X}-0x{4:04X}")]
    AddressOutOfRange(String, usize, usize, usize, usize),

    #[error("Memory section not found: {0}")]
    SectionNotFound(String),

    #[error("Address conflict: {0} at 0x{1:04X}-0x{2:04X} overlaps with existing allocation")]
    AddressConflict(String, u16, u16),

    // Binary generation errors (no Pos)
    #[error("Invalid instruction format")]
    InvalidInstructionFormat,

    #[error("Unresolved symbol: {0}")]
    UnresolvedSymbol(String),

    #[error("Invalid binary data")]
    InvalidBinaryData,

    // Assembly code generation errors
    #[error("{0}: Invalid instruction: {1}")]
    InvalidInstruction(Pos, String),

    #[error("{0}: Invalid register: {1}")]
    InvalidRegister(Pos, String),

    #[error("{0}: Invalid operand count for instruction {1}: expected {2}, got {3}")]
    InvalidOperandCount(Pos, String, usize, usize),

    #[error("{0}: Invalid operand type for instruction {1}")]
    InvalidOperandType(Pos, String),

    #[error("{0}: Undefined label: {1}")]
    UndefinedLabel(Pos, String),

    #[error("{0}: Invalid immediate value: {1}")]
    InvalidImmediate(Pos, String),

    #[error("{0}: Label redefinition: {1}")]
    LabelRedefinition(Pos, String),

    #[error("{0}: Cannot negate symbol")]
    CannotNegateSymbol(Pos),

    #[error("{0}: Dereference operations cannot be evaluated at assembly time")]
    CannotDereferenceInAssembly(Pos),

    #[error("{0}: Unknown symbol: {1}")]
    UnknownSymbol(Pos, String),

    #[error("{0}: Cannot access field of immediate value")]
    CannotAccessFieldOfImmediate(Pos),

    #[error("{0}: Cannot access field '{1}' of symbol '{2}'")]
    CannotAccessFieldOfSymbol(Pos, String, String),

    #[error("{0}: Array index must be a constant in assembly")]
    NonConstantArrayIndex(Pos),

    #[error("{0}: Cannot index immediate value")]
    CannotIndexImmediate(Pos),

    #[error("{0}: Cannot index label")]
    CannotIndexLabel(Pos),

    #[error("{0}: Cannot access field of label")]
    CannotAccessFieldOfLabel(Pos),

    #[error("{0}: Cannot perform arithmetic operations on labels")]
    CannotPerformArithmeticOnLabel(Pos),

    #[error("{0}: Cannot add two symbols")]
    CannotAddSymbols(Pos),

    #[error("{0}: Invalid subtraction in address expression")]
    InvalidSubtractionInAddress(Pos),

    #[error("{0}: Unsupported operation in address expression")]
    UnsupportedOperationInAddress(Pos),

    #[error("{0}: Cannot evaluate sizeof type: {1}")]
    CannotEvaluateSizeofType(Pos, String),

    #[error("{0}: Cannot evaluate sizeof expression: {1}")]
    CannotEvaluateSizeofExpr(Pos, String),

    #[error("{0}: Unsupported expression type in assembly: {1}")]
    UnsupportedExprType(Pos, String),

    #[error("{0}: Field '{1}' not found in struct")]
    FieldNotFoundInStruct(Pos, String),

    #[error("{0}: Type is not a struct")]
    TypeIsNotStruct(Pos),

    #[error("{0}: Type is not an array")]
    TypeIsNotArray(Pos),

    // Function code generation errors
    #[error("{0}: Type collection failed for: {1}")]
    TypeCollectionFailed(Pos, String),

    #[error("{0}: Invalid lvalue in assignment: {1}")]
    InvalidLValue(Pos, String),

    #[error("{0}: Unsupported expression type: {1}")]
    UnsupportedExpression(Pos, String),

    #[error("{0}: Unsupported statement type: {1}")]
    UnsupportedStatement(Pos, String),

    #[error("{0}: Undefined variable: {1}")]
    UndefinedVariable(Pos, String),

    #[error("{0}: Invalid function call: {1}")]
    InvalidFunctionCall(Pos, String),

    #[error("{0}: Unknown scope: '{1}")]
    UnknownScope(Pos, String),

    #[error("{0}: Expected a scope name after '{1}'")]
    ScopeRequired(Pos, String),

    // Evaluation errors
    #[error("{0}: Duplicate definition: {1}")]
    Duplicate(Pos, String),

    #[error("{0}: Missing type annotation for: {1}")]
    MissingTypeAnnotation(Pos, String),

    #[error("{0}: Unsupported const expression: {1:?}")]
    UnsupportedConstExpr(Pos, String),

    #[error("{0}: {1} is not a type")]
    NotAType(Pos, String),

    #[error("{0}: Unknown type: {1}")]
    UnknownType(Pos, String),

    #[error("{0}: Array length must be a constant integer")]
    NonConstantArrayLength(Pos),

    #[error("{0}: {1} is not a constant")]
    NotAConstant(Pos, String),

    #[error("{0}: Unknown constant: {1}")]
    UnknownConstant(Pos, String),

    #[error("{0}: Division by zero")]
    DivisionByZero(Pos),

    #[error("{0}: Modulo by zero")]
    ModuloByZero(Pos),

    #[error("{0}: Binary operation requires numeric operands")]
    NonNumericBinaryOperands(Pos),

    #[error("{0}: Unary operation requires numeric operand")]
    NonNumericUnaryOperand(Pos),

    #[error("{0}: Expression cannot be evaluated at compile time")]
    NonConstantExpression(Pos),

    #[error("{0}: Cannot infer type of empty array")]
    EmptyArrayTypeInference(Pos),

    #[error("{0}: {1} is not a value")]
    NotAValue(Pos, String),

    #[error("{0}: Unknown identifier: {1}")]
    UnknownIdentifier(Pos, String),

    #[error("{0}: Expression is not callable")]
    NotCallable(Pos),

    #[error("{0}: Expression is not indexable")]
    NotIndexable(Pos),

    #[error("{0}: Struct has no field: {1}")]
    NoSuchField(Pos, String),

    #[error("{0}: Expression is not a struct")]
    NotAStruct(Pos),

    #[error("{0}: Cannot dereference non-pointer type")]
    CannotDereferenceNonPointer(Pos),

    #[error("{0}: Cannot cast between types of different sizes: {1} bytes to {2} bytes")]
    InvalidCastSize(Pos, usize, usize),

    #[error("{0}: Expression is not addressable: {1}")]
    NotAddressable(Pos, String),

    #[error("{0}: Cannot dereference in static context")]
    CannotDereferenceInStaticContext(Pos),

    #[error("{0}: Invalid address operation")]
    InvalidAddressOperation(Pos),

    #[error("{0}: Address offset must be a constant")]
    NonConstantAddressOffset(Pos),

    #[error("{0}: Array index in address expression must be a constant")]
    NonConstantArrayIndexInAddress(Pos),

    #[error("{0}: Duplicate local variable: {1}")]
    DuplicateLocal(Pos, String),

    #[error("{0}: {1} is not an asm block")]
    NotAnAsm(Pos, String),

    #[error("{0}: {1} is not a function")]
    NotAFunction(Pos, String),

    #[error("{0}: {1} is not code generatable (not asm or func)")]
    NotCodeGeneratable(Pos, String),

    #[error("{0}: {1} is not a global label")]
    NotGlobalLabel(Pos, String),

    #[error("{0}: Undefined global label: {1}")]
    UndefinedGlobalLabel(Pos, String),

    #[error("{0}: Expected a global label")]
    GlobalLabelExpected(Pos),

    #[error("{0}: Undefined local label: {1}")]
    UndefinedLocalLabel(Pos, String),

    #[error("{0}: Expected a local label")]
    LocalLabelExpected(Pos),

    #[error("{0}: Static variable '{1}' cannot be used as immediate value directly. Use '{1}@' to get its address")]
    StaticRequiresAddressOf(Pos, String),

    #[error("{0}: '{1}' is not a valid immediate value")]
    InvalidImmediateValue(Pos, String),
}

impl Error {
    /// エラーの発生位置。IO・リンク・バイナリ生成系は位置を持たないため None。
    pub fn pos(&self) -> Option<&Pos> {
        use Error::*;
        match self {
            Io(_)
            | FixedAddressOverlapped(..)
            | SymbolNotFound(..)
            | InvalidSectionData
            | AddressSpaceOverflow(..)
            | AddressOutOfRange(..)
            | SectionNotFound(..)
            | AddressConflict(..)
            | InvalidInstructionFormat
            | UnresolvedSymbol(..)
            | InvalidBinaryData => None,

            InvalidToken(p, ..)
            | UnexpectedToken(p, ..)
            | UnexpectedEOF(p, ..)
            | InvalidType(p, ..)
            | InvalidFunction(p, ..)
            | InvalidVariable(p, ..)
            | InvalidInstruction(p, ..)
            | InvalidRegister(p, ..)
            | InvalidOperandCount(p, ..)
            | InvalidOperandType(p, ..)
            | UndefinedLabel(p, ..)
            | InvalidImmediate(p, ..)
            | LabelRedefinition(p, ..)
            | CannotNegateSymbol(p, ..)
            | CannotDereferenceInAssembly(p, ..)
            | UnknownSymbol(p, ..)
            | CannotAccessFieldOfImmediate(p, ..)
            | CannotAccessFieldOfSymbol(p, ..)
            | NonConstantArrayIndex(p, ..)
            | CannotIndexImmediate(p, ..)
            | CannotIndexLabel(p, ..)
            | CannotAccessFieldOfLabel(p, ..)
            | CannotPerformArithmeticOnLabel(p, ..)
            | CannotAddSymbols(p, ..)
            | InvalidSubtractionInAddress(p, ..)
            | UnsupportedOperationInAddress(p, ..)
            | CannotEvaluateSizeofType(p, ..)
            | CannotEvaluateSizeofExpr(p, ..)
            | UnsupportedExprType(p, ..)
            | FieldNotFoundInStruct(p, ..)
            | TypeIsNotStruct(p, ..)
            | TypeIsNotArray(p, ..)
            | TypeCollectionFailed(p, ..)
            | InvalidLValue(p, ..)
            | UnsupportedExpression(p, ..)
            | UnsupportedStatement(p, ..)
            | UndefinedVariable(p, ..)
            | InvalidFunctionCall(p, ..)
            | UnknownScope(p, ..)
            | ScopeRequired(p, ..)
            | Duplicate(p, ..)
            | MissingTypeAnnotation(p, ..)
            | UnsupportedConstExpr(p, ..)
            | NotAType(p, ..)
            | UnknownType(p, ..)
            | NonConstantArrayLength(p, ..)
            | NotAConstant(p, ..)
            | UnknownConstant(p, ..)
            | DivisionByZero(p, ..)
            | ModuloByZero(p, ..)
            | NonNumericBinaryOperands(p, ..)
            | NonNumericUnaryOperand(p, ..)
            | NonConstantExpression(p, ..)
            | EmptyArrayTypeInference(p, ..)
            | NotAValue(p, ..)
            | UnknownIdentifier(p, ..)
            | NotCallable(p, ..)
            | NotIndexable(p, ..)
            | NoSuchField(p, ..)
            | NotAStruct(p, ..)
            | CannotDereferenceNonPointer(p, ..)
            | InvalidCastSize(p, ..)
            | NotAddressable(p, ..)
            | CannotDereferenceInStaticContext(p, ..)
            | InvalidAddressOperation(p, ..)
            | NonConstantAddressOffset(p, ..)
            | NonConstantArrayIndexInAddress(p, ..)
            | DuplicateLocal(p, ..)
            | NotAnAsm(p, ..)
            | NotAFunction(p, ..)
            | NotCodeGeneratable(p, ..)
            | NotGlobalLabel(p, ..)
            | UndefinedGlobalLabel(p, ..)
            | GlobalLabelExpected(p, ..)
            | UndefinedLocalLabel(p, ..)
            | LocalLabelExpected(p, ..)
            | StaticRequiresAddressOf(p, ..)
            | InvalidImmediateValue(p, ..) => Some(p),
        }
    }
}
