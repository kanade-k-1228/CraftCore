use crate::grammer::token::{Pos, Token, TokenKind};
use std::fmt;
use thiserror::Error;

/// Owned token information (for storing in errors)
#[derive(Debug, Clone)]
pub struct TokenInfo {
    pub kind: TokenKind,
    pub pos: Pos,
}

impl fmt::Display for TokenInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} at {}", self.kind, self.pos)
    }
}

impl From<&Token> for TokenInfo {
    fn from(token: &Token) -> Self {
        TokenInfo {
            kind: token.kind.clone(),
            pos: token.pos.clone(),
        }
    }
}

impl From<Token> for TokenInfo {
    fn from(token: Token) -> Self {
        TokenInfo {
            kind: token.kind,
            pos: token.pos,
        }
    }
}

// Unified error type for TASM
#[derive(Debug, Error)]
pub enum Error {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    // Parse errors
    #[error("{1}: Unexpected token: {0}")]
    UnexpectedToken(TokenInfo, Pos),

    #[error("{0}: Unexpected end of file")]
    UnexpectedEOF(Pos),

    #[error("{1}: Invalid type: {0:?}")]
    InvalidType(String, Pos),

    #[error("{1}: Invalid function: {0:?}")]
    InvalidFunction(String, Pos),

    #[error("{1}: Invalid variable: {0:?}")]
    InvalidVariable(String, Pos),

    // Link errors (no Loc - operate on symbols)
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

    // Binary generation errors (no Loc)
    #[error("Invalid instruction format")]
    InvalidInstructionFormat,

    #[error("Unresolved symbol: {0}")]
    UnresolvedSymbol(String),

    #[error("Invalid binary data")]
    InvalidBinaryData,

    // Assembly code generation errors
    #[error("{1}: Invalid instruction: {0}")]
    InvalidInstruction(String, Pos),

    #[error("{1}: Invalid register: {0}")]
    InvalidRegister(String, Pos),

    #[error("{3}: Invalid operand count for instruction {0}: expected {1}, got {2}")]
    InvalidOperandCount(String, usize, usize, Pos),

    #[error("{1}: Invalid operand type for instruction {0}")]
    InvalidOperandType(String, Pos),

    #[error("{1}: Undefined label: {0}")]
    UndefinedLabel(String, Pos),

    #[error("{1}: Invalid immediate value: {0}")]
    InvalidImmediate(String, Pos),

    #[error("{1}: Label redefinition: {0}")]
    LabelRedefinition(String, Pos),

    #[error("{0}: Cannot negate symbol")]
    CannotNegateSymbol(Pos),

    #[error("{0}: Dereference operations cannot be evaluated at assembly time")]
    CannotDereferenceInAssembly(Pos),

    #[error("{1}: Unknown symbol: {0}")]
    UnknownSymbol(String, Pos),

    #[error("{0}: Cannot access field of immediate value")]
    CannotAccessFieldOfImmediate(Pos),

    #[error("{2}: Cannot access field '{0}' of symbol '{1}'")]
    CannotAccessFieldOfSymbol(String, String, Pos),

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

    #[error("{1}: Cannot evaluate sizeof type: {0}")]
    CannotEvaluateSizeofType(String, Pos),

    #[error("{1}: Cannot evaluate sizeof expression: {0}")]
    CannotEvaluateSizeofExpr(String, Pos),

    #[error("{1}: Unsupported expression type in assembly: {0}")]
    UnsupportedExprType(String, Pos),

    #[error("{1}: Field '{0}' not found in struct")]
    FieldNotFoundInStruct(String, Pos),

    #[error("{0}: Type is not a struct")]
    TypeIsNotStruct(Pos),

    #[error("{0}: Type is not an array")]
    TypeIsNotArray(Pos),

    // Function code generation errors
    #[error("{1}: Type collection failed for: {0}")]
    TypeCollectionFailed(String, Pos),

    #[error("{1}: Invalid lvalue in assignment: {0}")]
    InvalidLValue(String, Pos),

    #[error("{1}: Unsupported expression type: {0}")]
    UnsupportedExpression(String, Pos),

    #[error("{1}: Unsupported statement type: {0}")]
    UnsupportedStatement(String, Pos),

    #[error("{1}: Undefined variable: {0}")]
    UndefinedVariable(String, Pos),

    #[error("{1}: Invalid function call: {0}")]
    InvalidFunctionCall(String, Pos),

    // Evaluation errors
    #[error("{1}: Duplicate definition: {0}")]
    Duplicate(String, Pos),

    #[error("{1}: Missing type annotation for: {0}")]
    MissingTypeAnnotation(String, Pos),

    #[error("{1}: Unsupported const expression: {0:?}")]
    UnsupportedConstExpr(String, Pos),

    #[error("{1}: {0} is not a type")]
    NotAType(String, Pos),

    #[error("{1}: Unknown type: {0}")]
    UnknownType(String, Pos),

    #[error("{0}: Array length must be a constant integer")]
    NonConstantArrayLength(Pos),

    #[error("{1}: {0} is not a constant")]
    NotAConstant(String, Pos),

    #[error("{1}: Unknown constant: {0}")]
    UnknownConstant(String, Pos),

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

    #[error("{1}: {0} is not a value")]
    NotAValue(String, Pos),

    #[error("{1}: Unknown identifier: {0}")]
    UnknownIdentifier(String, Pos),

    #[error("{0}: Expression is not callable")]
    NotCallable(Pos),

    #[error("{0}: Expression is not indexable")]
    NotIndexable(Pos),

    #[error("{1}: Struct has no field: {0}")]
    NoSuchField(String, Pos),

    #[error("{0}: Expression is not a struct")]
    NotAStruct(Pos),

    #[error("{0}: Cannot dereference non-pointer type")]
    CannotDereferenceNonPointer(Pos),

    #[error("{2}: Cannot cast between types of different sizes: {0} bytes to {1} bytes")]
    InvalidCastSize(usize, usize, Pos),

    #[error("{1}: Expression is not addressable: {0}")]
    NotAddressable(String, Pos),

    #[error("{0}: Cannot dereference in static context")]
    CannotDereferenceInStaticContext(Pos),

    #[error("{0}: Invalid address operation")]
    InvalidAddressOperation(Pos),

    #[error("{0}: Address offset must be a constant")]
    NonConstantAddressOffset(Pos),

    #[error("{0}: Array index in address expression must be a constant")]
    NonConstantArrayIndexInAddress(Pos),

    #[error("{1}: Duplicate local variable: {0}")]
    DuplicateLocal(String, Pos),

    #[error("{1}: {0} is not an asm block")]
    NotAnAsm(String, Pos),

    #[error("{1}: {0} is not a function")]
    NotAFunction(String, Pos),

    #[error("{1}: {0} is not code generatable (not asm or func)")]
    NotCodeGeneratable(String, Pos),

    #[error("{1}: {0} is not a global label")]
    NotGlobalLabel(String, Pos),

    #[error("{1}: Undefined global label: {0}")]
    UndefinedGlobalLabel(String, Pos),

    #[error("{0}: Expected a global label")]
    GlobalLabelExpected(Pos),

    #[error("{1}: Undefined local label: {0}")]
    UndefinedLocalLabel(String, Pos),

    #[error("{0}: Expected a local label")]
    LocalLabelExpected(Pos),

    #[error("{1}: Static variable '{0}' cannot be used as immediate value directly. Use '{0}@' to get its address")]
    StaticRequiresAddressOf(String, Pos),

    #[error("{1}: '{0}' is not a valid immediate value")]
    InvalidImmediateValue(String, Pos),
}
