//! Error handling for the Lattice compiler

use thiserror::Error;

/// Result type for compiler operations
pub type CompilerResult<T> = Result<T, CompilerError>;

/// Main error type for the compiler
#[derive(Error, Debug)]
pub enum CompilerError {
  #[error("Lexical error: {0}")]
  Lexical(#[from] LexicalError),

  #[error("Parse error: {0}")]
  Parse(#[from] ParseError),

  #[error("Type error: {0}")]
  Type(#[from] TypeError),

  #[error("Code generation error: {0}")]
  CodeGen(#[from] CodeGenError),

  #[error("Internal error: {0}")]
  Internal(String),
}

/// Lexical analysis errors
#[derive(Error, Debug)]
pub enum LexicalError {
  #[error("Invalid character '{0}' at line {1}, column {2}")]
  InvalidCharacter(char, usize, usize),

  #[error("Unterminated string at line {0}, column {1}")]
  UnterminatedString(usize, usize),

  #[error("Invalid number format at line {0}, column {1}")]
  InvalidNumber(usize, usize),
}

/// Parsing errors
#[derive(Error, Debug)]
pub enum ParseError {
  #[error("Unexpected token '{0}' at line {1}, column {2}")]
  UnexpectedToken(String, usize, usize),

  #[error("Expected {0} but found {1} at line {2}, column {3}")]
  ExpectedToken(String, String, usize, usize),

  #[error("Unterminated expression at line {0}, column {1}")]
  UnterminatedExpression(usize, usize),
}

/// Type checking errors
#[derive(Error, Debug)]
pub enum TypeError {
  #[error("Type mismatch: expected {0}, got {1}")]
  TypeMismatch(String, String),

  #[error("Undefined identifier '{0}'")]
  UndefinedIdentifier(String),

  #[error("Circular type definition for '{0}'")]
  CircularType(String),
}

/// Code generation errors
#[derive(Error, Debug)]
pub enum CodeGenError {
  #[error("Unsupported feature: {0}")]
  UnsupportedFeature(String),

  #[error("Invalid target: {0}")]
  InvalidTarget(String),

  #[error("Code generation failed: {0}")]
  GenerationFailed(String),
}
