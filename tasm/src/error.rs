use thiserror::Error;

// Main error type for TASM
#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Parse(#[from] ParseError),

    #[error(transparent)]
    Collect(#[from] CollectError),

    #[error(transparent)]
    Link(#[from] LinkError),

    #[error(transparent)]
    Allocate(#[from] AllocateError),

    #[error(transparent)]
    BinGen(#[from] BinGenError),
}

// Parse errors
#[derive(Debug, Error, Clone)]
pub enum ParseError {
    #[error("TODO: not implemented yet")]
    TODO,

    #[error("Unexpected end of file")]
    UnexpectedEOF,

    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(String),

    #[error("Invalid type: {0:?}")]
    InvalidType(String),

    #[error("Invalid function: {0:?}")]
    InvalidFunction(String),

    #[error("Invalid variable: {0:?}")]
    InvalidVariable(String),
}

// Collection errors
#[derive(Debug, Error, Clone)]
pub enum CollectError {
    #[error("TODO: not implemented yet")]
    TODO,

    #[error("Duplicate definition: {0}")]
    Duplicate(String),

    #[error("Missing type annotation for: {0}")]
    MissingTypeAnnotation(String),

    #[error("Non-literal array length: {0:?}")]
    NonLiteralArrayLength(String),

    #[error("Unsupported const expression: {0:?}")]
    UnsupportedConstExpr(String),

    #[error("Cannot infer type of empty array")]
    CannotInferTypeOfEmptyArray,
}

// Link errors
#[derive(Debug, Error, Clone)]
pub enum LinkError {
    /// Address confilcted: {0} at 0x{1} to 0x{2}
    #[error("Address conflict: {0} at 0x{1:04X} to 0x{2:04X}")]
    FixedAddressOverlapped(String, u16, u16),

    #[error("Symbol not found: {0}")]
    SymbolNotFound(String),

    #[error("Invalid section data")]
    InvalidSectionData,

    #[error("Address space overflow: Cannot allocate {1} bytes for {0}")]
    AddressSpaceOverflow(String, u16),
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
