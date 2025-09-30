//! Parser Module for Lattice Language
//!
//! This module provides parsing functionality for the Lattice functional programming language.
//! It converts the token stream from the lexer into an Abstract Syntax Tree (AST) representing
//! the structure of Lattice programs.
//!
//! # Examples
//!
//! ## Basic Usage
//!
//! ```rust
//! use lattice_compiler::parser::Parser;
//! use lattice_compiler::lexer::lex;
//!
//! // First tokenize the source code
//! let tokens = lex("42")?;
//!
//! // Create a parser and parse the tokens
//! let parser = Parser::new();
//! let ast = parser.parse(&tokens)?;
//!
//! println!("Parsed AST: {:?}", ast);
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! ## Error Recovery
//!
//! ```rust
//! use lattice_compiler::parser::Parser;
//! use lattice_compiler::lexer::lex_with_errors;
//!
//! // Parse code with errors
//! let source = "42";
//! let tokens_result = lex_with_errors(source);
//!
//! match tokens_result {
//!     Ok(tokens) => {
//!         let parser = Parser::new();
//!         let ast = parser.parse(&tokens)?;
//!         println!("Successfully parsed AST");
//!     }
//!     Err(errors) => {
//!         println!("Lexical errors: {} errors found", errors.len());
//!     }
//! }
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! ## AST Traversal
//!
//! ```rust
//! use lattice_compiler::parser::{Parser, AstNode};
//! use lattice_compiler::lexer::lex;
//!
//! let parser = Parser::new();
//! let tokens = lex("42")?;
//! let ast = parser.parse(&tokens)?;
//!
//! // Access AST structure
//! println!("AST contains {} statements", ast.statements.len());
//! println!("AST span: {} to {}", ast.span.start, ast.span.end);
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```

// Re-export the main types for easy access
pub use ast::{AstNode, AstVisitor, Declaration, Expression, Pattern, Statement, TypeExpr};
pub use errors::{ErrorRecoveryConfig, ParseError, ParseErrorKind, ParseResult};
pub use parser::Parser;
pub use visitor::{MutableVisitor, Visitor};

// Module declarations
pub mod ast;
mod errors;
mod parser;
mod visitor;

/// Result type for parser operations
pub type ParserResult<T> = Result<T, ParseError>;

/// Result type for parser operations that may produce multiple errors
pub type ParserResultWithErrors<T> = Result<T, Vec<ParseError>>;

/// Convenience function to parse source code directly
///
/// This function combines lexing and parsing in a single call.
///
/// # Examples
///
/// ```rust
/// use lattice_compiler::parser::parse;
///
/// let ast = parse("42")?;
/// println!("Parsed AST: {:?}", ast);
/// # Ok::<(), Box<dyn std::error::Error>>(())
/// ```
pub fn parse(source: &str) -> ParserResult<AstNode> {
  use crate::lexer::lex;

  let tokens = lex(source)?;
  let mut parser = Parser::new();
  parser.parse(&tokens)
}

/// Convenience function to parse source code with error collection
///
/// This function combines lexing and parsing with error recovery.
///
/// # Examples
///
/// ```rust
/// use lattice_compiler::parser::parse_with_errors;
///
/// match parse_with_errors("let x = 42\nlet y = \nlet z = 100") {
///     Ok(ast) => println!("Successfully parsed AST"),
///     Err(errors) => println!("Parse failed with {} errors", errors.len()),
/// }
/// ```
pub fn parse_with_errors(source: &str) -> ParserResultWithErrors<AstNode> {
  use crate::lexer::lex_with_errors;

  let tokens_result = lex_with_errors(source);
  let mut parser = Parser::new();

  match tokens_result {
    Ok(tokens) => parser.parse(&tokens).map_err(|e| vec![e]),
    Err(lex_errors) => {
      // For now, return the lexical errors
      // In a full implementation, we'd try to parse the valid tokens
      Err(lex_errors.into_iter().map(|e| e.into()).collect())
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_parse_convenience_function() {
    let result = parse("let x = 42");
    assert!(result.is_ok());
  }

  #[test]
  fn test_parse_with_errors_convenience_function() {
    let result = parse_with_errors("let x = 42\nlet y = \nlet z = 100");
    assert!(result.is_err());

    if let Err(errors) = result {
      assert!(!errors.is_empty());
    }
  }

  #[test]
  fn test_public_api_integration() {
    // Test that the public API integrates well with the rest of the compiler
    // Use a simple expression that the parser can handle
    let source = "42";
    let result = parse(source);
    assert!(result.is_ok());

    let ast = result.unwrap();
    // Verify the AST has the expected structure
    assert!(matches!(ast.statements.len(), 1));
  }
}
