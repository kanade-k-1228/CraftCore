use crate::grammer::token::{Token, TokenKind};
use std::fmt;
use thiserror::Error;

// Token information without lifetime
#[derive(Debug, Clone)]
pub struct TokenInfo {
    pub kind: TokenKind,
    pub file: String,
    pub row: usize,
    pub col: usize,
}

impl fmt::Display for TokenInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} at {}:{}:{}",
            self.kind, self.file, self.row, self.col
        )
    }
}

impl<'a> From<Token<'a>> for TokenInfo {
    fn from(token: Token<'a>) -> Self {
        TokenInfo {
            kind: token.kind,
            file: token.pos.file.to_string(),
            row: token.pos.row,
            col: token.pos.col,
        }
    }
}

impl<'a> From<&Token<'a>> for TokenInfo {
    fn from(token: &Token<'a>) -> Self {
        TokenInfo {
            kind: token.kind.clone(),
            file: token.pos.file.to_string(),
            row: token.pos.row,
            col: token.pos.col,
        }
    }
}

// Main error type for TASM
#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Parse(#[from] ParseError),

    #[error(transparent)]
    Link(#[from] LinkError),

    #[error(transparent)]
    Allocate(#[from] AllocateError),

    #[error(transparent)]
    BinGen(#[from] BinGenError),

    #[error(transparent)]
    Asm(#[from] AsmError),

    #[error(transparent)]
    FuncGen(#[from] FuncGenError),

    #[error(transparent)]
    Eval(#[from] EvalError),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
}

// Parse errors
#[derive(Debug, Error, Clone)]
pub enum ParseError {
    #[error("TODO: not implemented yet")]
    TODO,

    #[error("Unexpected end of file")]
    UnexpectedEOF,

    #[error("Unexpected token: {0}")]
    UnexpectedToken(TokenInfo),

    #[error("Invalid type: {0:?}")]
    InvalidType(String),

    #[error("Invalid function: {0:?}")]
    InvalidFunction(String),

    #[error("Invalid variable: {0:?}")]
    InvalidVariable(String),
}

// Link errors
#[derive(Debug, Error, Clone)]
pub enum LinkError {
    /// Address confilcted: {0} at 0x{1} to 0x{2}
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
}

// Allocation errors
#[derive(Debug, Error, Clone)]
pub enum AllocateError {
    #[error("Address conflict: {0} at 0x{1:04X}-0x{2:04X} overlaps with existing allocation")]
    AddressConflict(String, u16, u16),

    #[error("Address space overflow: Cannot allocate {1} bytes for {0}")]
    AddressSpaceOverflow(String, u16),
}

// Binary generation errors
#[derive(Debug, Error, Clone)]
pub enum BinGenError {
    #[error("Invalid instruction format")]
    InvalidInstructionFormat,

    #[error("Unresolved symbol: {0}")]
    UnresolvedSymbol(String),

    #[error("Invalid binary data")]
    InvalidBinaryData,
}

// Assembly code generation errors
#[derive(Debug, Error, Clone)]
pub enum AsmError {
    #[error("Invalid instruction: {0}")]
    InvalidInstruction(String),

    #[error("Invalid register: {0}")]
    InvalidRegister(String),

    #[error("Invalid operand count for instruction {0}: expected {1}, got {2}")]
    InvalidOperandCount(String, usize, usize),

    #[error("Invalid operand type for instruction {0}")]
    InvalidOperandType(String),

    #[error("Undefined label: {0}")]
    UndefinedLabel(String),

    #[error("Invalid immediate value: {0}")]
    InvalidImmediate(String),

    #[error("Label redefinition: {0}")]
    LabelRedefinition(String),
}

// Function code generation errors
#[derive(Debug, Error, Clone)]
pub enum FuncGenError {
    #[error("Type collection failed for: {0}")]
    TypeCollectionFailed(String),

    #[error("Invalid lvalue in assignment: {0}")]
    InvalidLValue(String),

    #[error("Unsupported expression type: {0}")]
    UnsupportedExpression(String),

    #[error("Unsupported statement type: {0}")]
    UnsupportedStatement(String),

    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Invalid function call: {0}")]
    InvalidFunctionCall(String),
}

// Evaluation errors
#[derive(Debug, Error, Clone)]
pub enum EvalError {
    #[error("TODO: not implemented yet")]
    TODO,

    #[error("Duplicate definition: {0}")]
    Duplicate(String),

    #[error("Missing type annotation for: {0}")]
    MissingTypeAnnotation(String),

    #[error("Unsupported const expression: {0:?}")]
    UnsupportedConstExpr(String),

    #[error("{0} is not a type")]
    NotAType(String),

    #[error("Unknown type: {0}")]
    UnknownType(String),

    #[error("Array length must be a constant integer")]
    NonConstantArrayLength,

    #[error("{0} is not a constant")]
    NotAConstant(String),

    #[error("Unknown constant: {0}")]
    UnknownConstant(String),

    #[error("Division by zero")]
    DivisionByZero,

    #[error("Modulo by zero")]
    ModuloByZero,

    #[error("Binary operation requires numeric operands")]
    NonNumericBinaryOperands,

    #[error("Unary operation requires numeric operand")]
    NonNumericUnaryOperand,

    #[error("Expression cannot be evaluated at compile time")]
    NonConstantExpression,

    #[error("Cannot infer type of empty array")]
    EmptyArrayTypeInference,

    #[error("{0} is not a value")]
    NotAValue(String),

    #[error("Unknown identifier: {0}")]
    UnknownIdentifier(String),

    #[error("Expression is not callable")]
    NotCallable,

    #[error("Expression is not indexable")]
    NotIndexable,

    #[error("Struct has no field: {0}")]
    NoSuchField(String),

    #[error("Expression is not a struct")]
    NotAStruct,

    #[error("Cannot dereference non-pointer type")]
    CannotDereferenceNonPointer,

    #[error("Cannot cast between types of different sizes: {0} bytes to {1} bytes")]
    InvalidCastSize(usize, usize),
}
