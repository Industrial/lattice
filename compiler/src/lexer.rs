//! Lexical analysis for the Lattice language

use crate::error::CompilerResult;

/// Token types for the Lattice language
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
  // Keywords
  Let,
  Fn,
  If,
  Then,
  Else,
  Match,
  Effect,
  Handle,
  With,

  // Identifiers and literals
  Identifier(String),
  Integer(i64),
  Float(f64),
  String(String),
  Bool(bool),

  // Operators
  Plus,
  Minus,
  Times,
  Divide,
  Equal,
  Arrow,
  Colon,

  // Delimiters
  LParen,
  RParen,
  LBrace,
  RBrace,
  Comma,
  Semicolon,

  // Special
  EOF,
}

/// Lexer for the Lattice language
#[allow(dead_code)]
pub struct Lexer {
  source: String,
  position: usize,
  line: usize,
  column: usize,
}

impl Lexer {
  /// Create a new lexer for the given source code
  pub fn new(source: String) -> Self {
    Self {
      source,
      position: 0,
      line: 1,
      column: 1,
    }
  }

  /// Tokenize the source code
  pub fn tokenize(&mut self) -> CompilerResult<Vec<Token>> {
    // TODO: Implement actual tokenization
    todo!("Tokenization not yet implemented")
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_lexer_creation() {
    let source = "fn main = 42".to_string();
    let lexer = Lexer::new(source);
    assert_eq!(lexer.line, 1);
    assert_eq!(lexer.column, 1);
  }
}
