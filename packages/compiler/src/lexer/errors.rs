//! Error handling and recovery configuration for the Lattice language lexer.
//!
//! This module provides comprehensive error types and configuration options
//! for handling lexical errors during tokenization.

use crate::lexer::tokens::SourceLocation;
use std::fmt;

/// Represents a lexical error that occurred during tokenization
#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
  /// Invalid character encountered
  InvalidCharacter(char, SourceLocation),
  /// Unterminated string literal
  UnterminatedString(SourceLocation),
  /// Unterminated character literal
  UnterminatedChar(SourceLocation),
  /// Unterminated block comment
  UnterminatedBlockComment(SourceLocation),
  /// Unterminated line comment
  UnterminatedComment(SourceLocation),
  /// Invalid escape sequence
  InvalidEscapeSequence(String, SourceLocation),
  /// Invalid number format
  InvalidNumber(String, SourceLocation),
  /// Unexpected end of input
  UnexpectedEndOfInput(SourceLocation),
  /// Invalid token sequence
  InvalidTokenSequence(String, SourceLocation),
}

impl LexerError {
  /// Get the source location where this error occurred
  pub fn location(&self) -> SourceLocation {
    match self {
      LexerError::InvalidCharacter(_, loc) => *loc,
      LexerError::UnterminatedString(loc) => *loc,
      LexerError::UnterminatedChar(loc) => *loc,
      LexerError::UnterminatedBlockComment(loc) => *loc,
      LexerError::UnterminatedComment(loc) => *loc,
      LexerError::InvalidEscapeSequence(_, loc) => *loc,
      LexerError::InvalidNumber(_, loc) => *loc,
      LexerError::UnexpectedEndOfInput(loc) => *loc,
      LexerError::InvalidTokenSequence(_, loc) => *loc,
    }
  }

  /// Get a human-readable description of the error
  pub fn description(&self) -> &'static str {
    match self {
      LexerError::InvalidCharacter(_, _) => "Invalid character",
      LexerError::UnterminatedString(_) => "Unterminated string literal",
      LexerError::UnterminatedChar(_) => "Unterminated character literal",
      LexerError::UnterminatedBlockComment(_) => "Unterminated block comment",
      LexerError::UnterminatedComment(_) => "Unterminated line comment",
      LexerError::InvalidEscapeSequence(_, _) => "Invalid escape sequence",
      LexerError::InvalidNumber(_, _) => "Invalid number format",
      LexerError::UnexpectedEndOfInput(_) => "Unexpected end of input",
      LexerError::InvalidTokenSequence(_, _) => "Invalid token sequence",
    }
  }

  /// Get additional context information for the error
  pub fn context(&self) -> Option<String> {
    match self {
      LexerError::InvalidCharacter(ch, _) => Some(format!("Character: '{}'", ch)),
      LexerError::UnterminatedString(_) => Some("Missing closing quote".to_string()),
      LexerError::UnterminatedChar(_) => Some("Missing closing single quote".to_string()),
      LexerError::UnterminatedBlockComment(_) => Some("Missing closing '*/'".to_string()),
      LexerError::UnterminatedComment(_) => Some("Missing newline".to_string()),
      LexerError::InvalidEscapeSequence(seq, _) => Some(format!("Sequence: '{}'", seq)),
      LexerError::InvalidNumber(num, _) => Some(format!("Number: '{}'", num)),
      LexerError::UnexpectedEndOfInput(_) => Some("File ended unexpectedly".to_string()),
      LexerError::InvalidTokenSequence(seq, _) => Some(format!("Sequence: '{}'", seq)),
    }
  }
}

impl fmt::Display for LexerError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let location = self.location();
    let description = self.description();

    write!(f, "{} at {}", description, location)?;

    if let Some(context) = self.context() {
      write!(f, " ({})", context)?;
    }

    Ok(())
  }
}

impl std::error::Error for LexerError {
  fn description(&self) -> &str {
    self.description()
  }
}

/// Configuration for error recovery behavior during lexing
#[derive(Debug, Clone, PartialEq)]
pub struct ErrorRecoveryConfig {
  /// Whether to continue lexing after encountering an error
  pub continue_on_error: bool,
  /// Maximum number of errors to collect before stopping
  pub max_errors: Option<usize>,
  /// Whether to insert InvalidToken tokens for errors
  pub insert_error_tokens: bool,
  /// Whether to skip invalid characters during recovery
  pub skip_invalid_chars: bool,
}

impl Default for ErrorRecoveryConfig {
  fn default() -> Self {
    Self {
      continue_on_error: true,
      max_errors: Some(100),
      insert_error_tokens: true,
      skip_invalid_chars: false,
    }
  }
}

impl ErrorRecoveryConfig {
  /// Create a new error recovery configuration
  pub fn new() -> Self {
    Self::default()
  }

  /// Create a strict error recovery configuration that stops on first error
  pub fn strict() -> Self {
    Self {
      continue_on_error: false,
      max_errors: Some(1),
      insert_error_tokens: false,
      skip_invalid_chars: false,
    }
  }

  /// Create a permissive error recovery configuration
  pub fn permissive() -> Self {
    Self {
      continue_on_error: true,
      max_errors: None,
      insert_error_tokens: true,
      skip_invalid_chars: true,
    }
  }

  /// Set whether to continue on error
  pub fn continue_on_error(mut self, continue_on_error: bool) -> Self {
    self.continue_on_error = continue_on_error;
    self
  }

  /// Set the maximum number of errors
  pub fn max_errors(mut self, max_errors: Option<usize>) -> Self {
    self.max_errors = max_errors;
    self
  }

  /// Set whether to insert error tokens
  pub fn insert_error_tokens(mut self, insert_error_tokens: bool) -> Self {
    self.insert_error_tokens = insert_error_tokens;
    self
  }

  /// Set whether to skip invalid characters
  pub fn skip_invalid_chars(mut self, skip_invalid_chars: bool) -> Self {
    self.skip_invalid_chars = skip_invalid_chars;
    self
  }

  /// Validate that the configuration is reasonable
  pub fn validate(&self) -> Result<(), String> {
    if self.max_errors == Some(0) {
      return Err("max_errors cannot be 0".to_string());
    }

    if !self.continue_on_error && self.max_errors.is_some() {
      return Err("max_errors is ignored when continue_on_error is false".to_string());
    }

    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_lexer_error_location() {
    let loc = SourceLocation::new(5, 10, 100);
    let error = LexerError::InvalidCharacter('@', loc);
    assert_eq!(error.location(), loc);
  }

  #[test]
  fn test_lexer_error_description() {
    let loc = SourceLocation::new(1, 1, 0);
    let error = LexerError::UnterminatedString(loc);
    assert_eq!(error.description(), "Unterminated string literal");
  }

  #[test]
  fn test_lexer_error_context() {
    let loc = SourceLocation::new(1, 1, 0);
    let error = LexerError::InvalidCharacter('@', loc);
    assert_eq!(error.context(), Some("Character: '@'".to_string()));
  }

  #[test]
  fn test_lexer_error_display() {
    let loc = SourceLocation::new(5, 10, 100);
    let error = LexerError::InvalidCharacter('@', loc);
    assert_eq!(
      error.to_string(),
      "Invalid character at 5:10 (Character: '@')"
    );
  }

  #[test]
  fn test_error_recovery_config_default() {
    let config = ErrorRecoveryConfig::default();
    assert!(config.continue_on_error);
    assert_eq!(config.max_errors, Some(100));
    assert!(config.insert_error_tokens);
    assert!(!config.skip_invalid_chars);
  }

  #[test]
  fn test_error_recovery_config_strict() {
    let config = ErrorRecoveryConfig::strict();
    assert!(!config.continue_on_error);
    assert_eq!(config.max_errors, Some(1));
    assert!(!config.insert_error_tokens);
    assert!(!config.skip_invalid_chars);
  }

  #[test]
  fn test_error_recovery_config_permissive() {
    let config = ErrorRecoveryConfig::permissive();
    assert!(config.continue_on_error);
    assert_eq!(config.max_errors, None);
    assert!(config.insert_error_tokens);
    assert!(config.skip_invalid_chars);
  }

  #[test]
  fn test_error_recovery_config_builder() {
    let config = ErrorRecoveryConfig::new()
      .continue_on_error(false)
      .max_errors(Some(50))
      .insert_error_tokens(false)
      .skip_invalid_chars(true);

    assert!(!config.continue_on_error);
    assert_eq!(config.max_errors, Some(50));
    assert!(!config.insert_error_tokens);
    assert!(config.skip_invalid_chars);
  }

  #[test]
  fn test_error_recovery_config_validation() {
    let config = ErrorRecoveryConfig::default();
    assert!(config.validate().is_ok());

    let invalid_config = ErrorRecoveryConfig {
      max_errors: Some(0),
      ..Default::default()
    };
    assert!(invalid_config.validate().is_err());
  }
}
