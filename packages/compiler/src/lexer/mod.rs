//! Lexical Analysis Module for Lattice Language
//!
//! This module provides lexical analysis functionality for the Lattice functional programming language.
//! It includes token definitions, source location tracking, error handling, and a clean public API
//! for the lexer that integrates seamlessly with the rest of the compiler.
//!
//! # Examples
//!
//! ## Basic Usage
//!
//! ```rust
//! use lattice_compiler::lexer::{Lexer, LexerConfig};
//!
//! // Create a lexer with default configuration
//! let mut lexer = Lexer::from_str("let x = 42 in x + 1");
//!
//! // Tokenize the source code
//! match lexer.tokenize() {
//!     Ok(tokens) => {
//!         for token in tokens {
//!             println!("Token: {:?} at {}", token.kind, token.start);
//!         }
//!     }
//!     Err(error) => {
//!         eprintln!("Lexical error: {}", error);
//!     }
//! }
//! ```
//!
//! ## Advanced Usage with Error Recovery
//!
//! ```rust
//! use lattice_compiler::lexer::{Lexer, LexerConfig, ErrorRecoveryConfig};
//!
//! // Create a lexer with custom error recovery configuration
//! let config = LexerConfig {
//!     error_recovery: ErrorRecoveryConfig {
//!         continue_on_error: true,
//!         max_errors: Some(100),
//!         insert_error_tokens: true,
//!         skip_invalid_chars: false,
//!     },
//!     include_comments: false,
//!     include_whitespace: false,
//! };
//!
//! let mut lexer = Lexer::with_config("let x = \u{0000} y = 42".to_string(), config);
//!
//! // Collect all errors while continuing to process valid tokens
//! match lexer.tokenize_with_errors() {
//!     Ok(tokens) => {
//!         println!("Successfully tokenized {} tokens", tokens.len());
//!     }
//!     Err(errors) => {
//!         println!("Encountered {} errors:", errors.len());
//!         for error in &errors {
//!             println!("  - {} at {}", error, error.location());
//!         }
//!         
//!         // Get a summary of all errors
//!         let summary = lexer.get_error_summary(&errors);
//!         println!("Error summary:\n{}", summary);
//!     }
//! }
//! ```
//!
//! ## Iterator Interface
//!
//! ```rust
//! use lattice_compiler::lexer::Lexer;
//!
//! let mut lexer = Lexer::from_str("let x = 42");
//!
//! // Use the lexer as an iterator
//! for token in lexer {
//!     println!("Token: {:?}", token);
//! }
//! ```
//!
//! ## Source Location Tracking
//!
//! ```rust
//! use lattice_compiler::lexer::{Lexer, SourceLocation};
//!
//! let mut lexer = Lexer::from_str("let x = 42\nlet y = 100");
//!
//! if let Ok(tokens) = lexer.tokenize() {
//!     for token in tokens {
//!         let (start, end) = token.span();
//!         println!("Token '{}' spans from {} to {}",
//!                 token.text, start, end);
//!     }
//! }
//! ```

// Re-export the main types for easy access
pub use errors::{ErrorRecoveryConfig, LexerError};
pub use lexer::{Lexer, LexerBuilder, LexerConfig};
pub use tokens::{SourceLocation, Token, TokenKind};

// Re-export common traits and utilities
pub use traits::{
  ErrorHandler, LexerBuilder as LexerBuilderTrait, LexerConfig as LexerConfigTrait, TokenStream,
};

// Module declarations
mod errors;
mod lexer;
mod tokens;
mod traits;

// Re-export the logos-generated TokenKind for backward compatibility
// This ensures existing code continues to work
pub use logos::Logos;

/// Result type for lexer operations
pub type LexerResult<T> = Result<T, LexerError>;

/// Result type for lexer operations that may produce multiple errors
pub type LexerResultWithErrors<T> = Result<T, Vec<LexerError>>;

/// Convenience function to create a lexer from a string slice
///
/// This is equivalent to `Lexer::from_str(source)` but provides
/// a more ergonomic API for quick lexing tasks.
///
/// # Examples
///
/// ```rust
/// use lattice_compiler::lexer::lex;
///
/// let tokens = lex("let x = 42")?;
/// # Ok::<(), lattice_compiler::lexer::LexerError>(())
/// ```
pub fn lex(source: &str) -> LexerResult<Vec<Token>> {
  Lexer::from_str(source).tokenize()
}

/// Convenience function to create a lexer from a string slice with error collection
///
/// This is equivalent to `Lexer::from_str(source).tokenize_with_errors()` but provides
/// a more ergonomic API for quick lexing tasks with error recovery.
///
/// # Examples
///
/// ```rust
/// use lattice_compiler::lexer::lex_with_errors;
///
/// match lex_with_errors("let x = \u{0000} y = 42") {
///     Ok(tokens) => println!("Successfully tokenized {} tokens", tokens.len()),
///     Err(errors) => println!("Encountered {} errors", errors.len()),
/// }
/// ```
pub fn lex_with_errors(source: &str) -> LexerResultWithErrors<Vec<Token>> {
  Lexer::from_str(source).tokenize_with_errors()
}

/// Convenience function to create a lexer from a string slice with custom configuration
///
/// This is equivalent to `Lexer::with_config(source, config)` but provides
/// a more ergonomic API for quick lexing tasks with custom settings.
///
/// # Examples
///
/// ```rust
/// use lattice_compiler::lexer::{lex_with_config, LexerConfig, ErrorRecoveryConfig};
///
/// let config = LexerConfig {
///     error_recovery: ErrorRecoveryConfig {
///         continue_on_error: true,
///         max_errors: Some(50),
///         insert_error_tokens: true,
///         skip_invalid_chars: false,
///     },
///     include_comments: true,
///     include_whitespace: false,
/// };
///
/// let tokens = lex_with_config("let x = 42", config)?;
/// # Ok::<(), lattice_compiler::lexer::LexerError>(())
/// ```
pub fn lex_with_config(source: &str, config: LexerConfig) -> LexerResult<Vec<Token>> {
  Lexer::with_config(source.to_string(), config).tokenize()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_lex_convenience_function() {
    let result = lex("let x = 42");
    assert!(result.is_ok());

    let tokens = result.unwrap();
    assert!(!tokens.is_empty());
  }

  #[test]
  fn test_lex_with_errors_convenience_function() {
    let result = lex_with_errors("let x = \u{0000} y = 42");
    assert!(result.is_err());

    if let Err(errors) = result {
      assert!(!errors.is_empty());
    }
  }

  #[test]
  fn test_lex_with_config_convenience_function() {
    let config = LexerConfig::default();
    let result = lex_with_config("let x = 42", config);
    assert!(result.is_ok());

    let tokens = result.unwrap();
    assert!(!tokens.is_empty());
  }

  #[test]
  fn test_public_api_integration() {
    // Test that the public API integrates well with the rest of the compiler
    let source = "let x = 42 in x + 1";
    let mut lexer = Lexer::from_str(source);

    // Test basic tokenization
    let result = lexer.tokenize();
    assert!(result.is_ok());

    // Test error handling
    let error_source = "let x = \u{0000} y = 42";
    let mut error_lexer = Lexer::from_str(error_source);
    let error_result = error_lexer.tokenize();
    assert!(error_result.is_err());

    // Test error recovery
    let recovery_result = error_lexer.tokenize_with_errors();
    assert!(recovery_result.is_err());

    if let Err(errors) = recovery_result {
      assert!(!errors.is_empty());
      let summary = error_lexer.get_error_summary(&errors);
      assert!(!summary.is_empty());
    }
  }
}
