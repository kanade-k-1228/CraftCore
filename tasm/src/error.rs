use crate::grammer::token::{Pos, Token};

/// TASM 共通エラー。発生位置 (pos) と内容 (kind) を分離して持つ。
/// 位置付きで作るときは `ErrorKind::X(..).at(pos)`、位置を持たないもの
/// (IO・リンク・バイナリ生成系) は `ErrorKind::X(..).into()`。
#[derive(Debug)]
pub struct Error {
    pub pos: Option<Pos>,
    pub kind: ErrorKind,
}

impl Error {
    /// エラーの発生位置。IO・リンク・バイナリ生成系は位置を持たないため None。
    pub fn pos(&self) -> Option<&Pos> {
        self.pos.as_ref()
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.pos {
            Some(pos) => write!(f, "{}: {}", pos, self.kind),
            None => write!(f, "{}", self.kind),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        std::error::Error::source(&self.kind)
    }
}

impl From<ErrorKind> for Error {
    fn from(kind: ErrorKind) -> Self {
        Error { pos: None, kind }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        ErrorKind::Io(e).into()
    }
}

impl ErrorKind {
    /// 発生位置を付けて Error にする。
    pub fn at(self, pos: Pos) -> Error {
        Error {
            pos: Some(pos),
            kind: self,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ErrorKind {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    // Parse errors
    #[error("Invalid token: {0:?}")]
    InvalidToken(String),

    #[error("Unexpected token: {0}")]
    UnexpectedToken(Token),

    #[error("Unexpected end of file")]
    UnexpectedEOF,

    // Link errors
    #[error("Address conflict: {0} at 0x{1:04X} to 0x{2:04X}")]
    FixedAddressOverlapped(String, usize, usize),

    #[error("Address space overflow: Cannot allocate {1} bytes for {0}")]
    AddressSpaceOverflow(String, usize),

    #[error("Address out of range: {0} at 0x{1:04X}-0x{2:04X} is outside allowed range 0x{3:04X}-0x{4:04X}")]
    AddressOutOfRange(String, usize, usize, usize, usize),

    #[error("Memory section not found: {0}")]
    SectionNotFound(String),

    // Assembly code generation errors
    #[error("Invalid instruction: {0}")]
    InvalidInstruction(String),

    #[error("Invalid register: {0}")]
    InvalidRegister(String),

    #[error("Invalid immediate value: {0}")]
    InvalidImmediate(String),

    #[error("Cannot negate symbol")]
    CannotNegateSymbol,

    #[error("Unknown symbol: {0}")]
    UnknownSymbol(String),

    #[error("Cannot access field of immediate value")]
    CannotAccessFieldOfImmediate,

    #[error("Array index must be a constant in assembly")]
    NonConstantArrayIndex,

    #[error("Cannot index immediate value")]
    CannotIndexImmediate,

    #[error("Cannot index label")]
    CannotIndexLabel,

    #[error("Cannot access field of label")]
    CannotAccessFieldOfLabel,

    #[error("Cannot perform arithmetic operations on labels")]
    CannotPerformArithmeticOnLabel,

    #[error("Cannot add two symbols")]
    CannotAddSymbols,

    #[error("Invalid subtraction in address expression")]
    InvalidSubtractionInAddress,

    #[error("Unsupported operation in address expression")]
    UnsupportedOperationInAddress,

    #[error("Cannot evaluate sizeof type: {0}")]
    CannotEvaluateSizeofType(String),

    #[error("Cannot evaluate sizeof expression: {0}")]
    CannotEvaluateSizeofExpr(String),

    #[error("Unsupported expression type in assembly: {0}")]
    UnsupportedExprType(String),

    #[error("Field '{0}' not found in struct")]
    FieldNotFoundInStruct(String),

    #[error("Type is not an array")]
    TypeIsNotArray,

    // Function code generation errors
    #[error("Type collection failed for: {0}")]
    TypeCollectionFailed(String),

    #[error("Invalid lvalue in assignment: {0}")]
    InvalidLValue(String),

    #[error("Unsupported expression type: {0}")]
    UnsupportedExpression(String),

    #[error("Unknown scope: '{0}")]
    UnknownScope(String),

    #[error("Expected a scope name after '{0}'")]
    ScopeRequired(String),

    // Evaluation errors
    #[error("Duplicate definition: {0}")]
    Duplicate(String),

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

    #[error("Expression is not addressable: {0}")]
    NotAddressable(String),

    #[error("Array index in address expression must be a constant")]
    NonConstantArrayIndexInAddress,

    #[error("Duplicate local variable: {0}")]
    DuplicateLocal(String),

    #[error("{0} is not an asm block")]
    NotAnAsm(String),

    #[error("{0} is not a function")]
    NotAFunction(String),

    #[error("{0} is not code generatable (not asm or func)")]
    NotCodeGeneratable(String),

    #[error("{0} is not a global label")]
    NotGlobalLabel(String),

    #[error("Undefined global label: {0}")]
    UndefinedGlobalLabel(String),

    #[error("Expected a global label")]
    GlobalLabelExpected,

    #[error("Undefined local label: {0}")]
    UndefinedLocalLabel(String),

    #[error("Expected a local label")]
    LocalLabelExpected,

    #[error("Static variable '{0}' cannot be used as immediate value directly. Use '{0}@' to get its address")]
    StaticRequiresAddressOf(String),

    #[error("'{0}' is not a valid immediate value")]
    InvalidImmediateValue(String),
}
