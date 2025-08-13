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
      max_errors: None,
      insert_error_tokens: false,
      skip_invalid_chars: false,
    }
  }

  /// Create a strict error recovery configuration with a safety net to prevent infinite loops
  pub fn strict_with_safety_net() -> Self {
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

    // When continue_on_error is false, max_errors is still meaningful
    // as a safety net to prevent infinite loops, so we allow it
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  // Test all LexerError variants for location() method
  #[test]
  fn test_lexer_error_location() {
    let loc = SourceLocation::new(5, 10, 100);
    let error = LexerError::InvalidCharacter('@', loc);
    assert_eq!(error.location(), loc);
  }

  #[test]
  fn test_all_lexer_error_variants_location() {
    let loc = SourceLocation::new(5, 10, 100);

    let errors = vec![
      LexerError::InvalidCharacter('@', loc),
      LexerError::UnterminatedString(loc),
      LexerError::UnterminatedChar(loc),
      LexerError::UnterminatedBlockComment(loc),
      LexerError::UnterminatedComment(loc),
      LexerError::InvalidEscapeSequence("\\x".to_string(), loc),
      LexerError::InvalidNumber("12.34.56".to_string(), loc),
      LexerError::UnexpectedEndOfInput(loc),
      LexerError::InvalidTokenSequence("+++".to_string(), loc),
    ];

    for error in errors {
      assert_eq!(error.location(), loc);
    }
  }

  // Test all LexerError variants for description() method
  #[test]
  fn test_lexer_error_description() {
    let loc = SourceLocation::new(1, 1, 0);
    let error = LexerError::UnterminatedString(loc);
    assert_eq!(error.description(), "Unterminated string literal");
  }

  #[test]
  fn test_all_lexer_error_variants_description() {
    let loc = SourceLocation::new(1, 1, 0);

    let expected_descriptions = vec![
      ("Invalid character", LexerError::InvalidCharacter('@', loc)),
      (
        "Unterminated string literal",
        LexerError::UnterminatedString(loc),
      ),
      (
        "Unterminated character literal",
        LexerError::UnterminatedChar(loc),
      ),
      (
        "Unterminated block comment",
        LexerError::UnterminatedBlockComment(loc),
      ),
      (
        "Unterminated line comment",
        LexerError::UnterminatedComment(loc),
      ),
      (
        "Invalid escape sequence",
        LexerError::InvalidEscapeSequence("\\x".to_string(), loc),
      ),
      (
        "Invalid number format",
        LexerError::InvalidNumber("12.34.56".to_string(), loc),
      ),
      (
        "Unexpected end of input",
        LexerError::UnexpectedEndOfInput(loc),
      ),
      (
        "Invalid token sequence",
        LexerError::InvalidTokenSequence("+++".to_string(), loc),
      ),
    ];

    for (expected, error) in expected_descriptions {
      assert_eq!(error.description(), expected);
    }
  }

  // Test all LexerError variants for context() method
  #[test]
  fn test_lexer_error_context() {
    let loc = SourceLocation::new(1, 1, 0);
    let error = LexerError::InvalidCharacter('@', loc);
    assert_eq!(error.context(), Some("Character: '@'".to_string()));
  }

  #[test]
  fn test_all_lexer_error_variants_context() {
    let loc = SourceLocation::new(1, 1, 0);

    let expected_contexts = vec![
      ("Character: '@'", LexerError::InvalidCharacter('@', loc)),
      ("Missing closing quote", LexerError::UnterminatedString(loc)),
      (
        "Missing closing single quote",
        LexerError::UnterminatedChar(loc),
      ),
      (
        "Missing closing '*/'",
        LexerError::UnterminatedBlockComment(loc),
      ),
      ("Missing newline", LexerError::UnterminatedComment(loc)),
      (
        "Sequence: '\\x'",
        LexerError::InvalidEscapeSequence("\\x".to_string(), loc),
      ),
      (
        "Number: '12.34.56'",
        LexerError::InvalidNumber("12.34.56".to_string(), loc),
      ),
      (
        "File ended unexpectedly",
        LexerError::UnexpectedEndOfInput(loc),
      ),
      (
        "Sequence: '+++'",
        LexerError::InvalidTokenSequence("+++".to_string(), loc),
      ),
    ];

    for (expected, error) in expected_contexts {
      assert_eq!(error.context(), Some(expected.to_string()));
    }
  }

  // Test Display implementation for all variants
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
  fn test_all_lexer_error_variants_display() {
    let loc = SourceLocation::new(3, 7, 50);

    let test_cases = vec![
      (
        LexerError::InvalidCharacter('$', loc),
        "Invalid character at 3:7 (Character: '$')",
      ),
      (
        LexerError::UnterminatedString(loc),
        "Unterminated string literal at 3:7 (Missing closing quote)",
      ),
      (
        LexerError::UnterminatedChar(loc),
        "Unterminated character literal at 3:7 (Missing closing single quote)",
      ),
      (
        LexerError::UnterminatedBlockComment(loc),
        "Unterminated block comment at 3:7 (Missing closing '*/')",
      ),
      (
        LexerError::UnterminatedComment(loc),
        "Unterminated line comment at 3:7 (Missing newline)",
      ),
      (
        LexerError::InvalidEscapeSequence("\\z".to_string(), loc),
        "Invalid escape sequence at 3:7 (Sequence: '\\z')",
      ),
      (
        LexerError::InvalidNumber("1e2e3".to_string(), loc),
        "Invalid number format at 3:7 (Number: '1e2e3')",
      ),
      (
        LexerError::UnexpectedEndOfInput(loc),
        "Unexpected end of input at 3:7 (File ended unexpectedly)",
      ),
      (
        LexerError::InvalidTokenSequence("---".to_string(), loc),
        "Invalid token sequence at 3:7 (Sequence: '---')",
      ),
    ];

    for (error, expected) in test_cases {
      assert_eq!(error.to_string(), expected);
    }
  }

  // Test Error trait implementation
  #[test]
  fn test_lexer_error_std_error_trait() {
    let loc = SourceLocation::new(1, 1, 0);
    let error = LexerError::InvalidCharacter('@', loc);

    // Test that the error implements std::error::Error
    let error_ref: &dyn std::error::Error = &error;
    assert_eq!(error_ref.description(), "Invalid character");
  }

  #[test]
  fn test_all_lexer_error_variants_std_error_trait() {
    let loc = SourceLocation::new(1, 1, 0);

    let test_cases = vec![
      (LexerError::InvalidCharacter('@', loc), "Invalid character"),
      (
        LexerError::UnterminatedString(loc),
        "Unterminated string literal",
      ),
      (
        LexerError::UnterminatedChar(loc),
        "Unterminated character literal",
      ),
      (
        LexerError::UnterminatedBlockComment(loc),
        "Unterminated block comment",
      ),
      (
        LexerError::UnterminatedComment(loc),
        "Unterminated line comment",
      ),
      (
        LexerError::InvalidEscapeSequence("\\x".to_string(), loc),
        "Invalid escape sequence",
      ),
      (
        LexerError::InvalidNumber("12.34.56".to_string(), loc),
        "Invalid number format",
      ),
      (
        LexerError::UnexpectedEndOfInput(loc),
        "Unexpected end of input",
      ),
      (
        LexerError::InvalidTokenSequence("+++".to_string(), loc),
        "Invalid token sequence",
      ),
    ];

    for (error, expected_description) in test_cases {
      let error_ref: &dyn std::error::Error = &error;
      assert_eq!(error_ref.description(), expected_description);
    }
  }

  // Test Clone and PartialEq for LexerError
  #[test]
  fn test_lexer_error_clone_and_partial_eq() {
    let loc = SourceLocation::new(1, 1, 0);
    let error1 = LexerError::InvalidCharacter('@', loc);
    let error2 = error1.clone();

    assert_eq!(error1, error2);
    assert!(error1 == error2);
    assert!(!(error1 != error2));
  }

  // Test Debug for LexerError
  #[test]
  fn test_lexer_error_debug() {
    let loc = SourceLocation::new(1, 1, 0);
    let error = LexerError::InvalidCharacter('@', loc);
    let debug_str = format!("{:?}", error);

    assert!(debug_str.contains("InvalidCharacter"));
    assert!(debug_str.contains('@'));
  }

  // ErrorRecoveryConfig tests
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
    assert_eq!(config.max_errors, None);
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
  fn test_error_recovery_config_new() {
    let config = ErrorRecoveryConfig::new();
    assert_eq!(config, ErrorRecoveryConfig::default());
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
  fn test_error_recovery_config_builder_chaining() {
    let config = ErrorRecoveryConfig::new()
      .continue_on_error(false)
      .max_errors(Some(25))
      .insert_error_tokens(true)
      .skip_invalid_chars(false);

    assert!(!config.continue_on_error);
    assert_eq!(config.max_errors, Some(25));
    assert!(config.insert_error_tokens);
    assert!(!config.skip_invalid_chars);
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

    let valid_config2 = ErrorRecoveryConfig {
      continue_on_error: false,
      max_errors: Some(100),
      ..Default::default()
    };
    assert!(valid_config2.validate().is_ok());
  }

  #[test]
  fn test_error_recovery_config_validation_edge_cases() {
    // Test with max_errors = Some(0)
    let config = ErrorRecoveryConfig {
      max_errors: Some(0),
      ..Default::default()
    };
    assert!(config.validate().is_err());

    // Test with continue_on_error = false and max_errors = Some(1)
    let config = ErrorRecoveryConfig {
      continue_on_error: false,
      max_errors: Some(1),
      ..Default::default()
    };
    assert!(config.validate().is_ok());

    // Test with continue_on_error = false and max_errors = None
    let config = ErrorRecoveryConfig {
      continue_on_error: false,
      max_errors: None,
      ..Default::default()
    };
    assert!(config.validate().is_ok());

    // Test with continue_on_error = true and max_errors = Some(1)
    let config = ErrorRecoveryConfig {
      continue_on_error: true,
      max_errors: Some(1),
      ..Default::default()
    };
    assert!(config.validate().is_ok());
  }

  // Test Clone and PartialEq for ErrorRecoveryConfig
  #[test]
  fn test_error_recovery_config_clone_and_partial_eq() {
    let config1 = ErrorRecoveryConfig::strict();
    let config2 = config1.clone();

    assert_eq!(config1, config2);
    assert!(config1 == config2);
    assert!(!(config1 != config2));
  }

  // Test Debug for ErrorRecoveryConfig
  #[test]
  fn test_error_recovery_config_debug() {
    let config = ErrorRecoveryConfig::default();
    let debug_str = format!("{:?}", config);

    assert!(debug_str.contains("ErrorRecoveryConfig"));
    assert!(debug_str.contains("continue_on_error"));
    assert!(debug_str.contains("max_errors"));
    assert!(debug_str.contains("insert_error_tokens"));
    assert!(debug_str.contains("skip_invalid_chars"));
  }

  // Test Display implementation edge cases
  #[test]
  fn test_lexer_error_display_without_context() {
    // This test would require a LexerError variant that returns None from context()
    // Since all current variants return Some, this test verifies the Display logic
    let loc = SourceLocation::new(1, 1, 0);
    let error = LexerError::InvalidCharacter('@', loc);

    let display_str = error.to_string();
    assert!(display_str.contains("Invalid character at 1:1"));
    assert!(display_str.contains("Character: '@'"));
  }

  // Test SourceLocation edge cases
  #[test]
  fn test_lexer_error_with_zero_location() {
    let loc = SourceLocation::new(0, 0, 0);
    let error = LexerError::InvalidCharacter('@', loc);

    assert_eq!(error.location(), loc);
    assert_eq!(
      error.to_string(),
      "Invalid character at 0:0 (Character: '@')"
    );
  }

  #[test]
  fn test_lexer_error_with_large_location() {
    let loc = SourceLocation::new(9999, 9999, 999999);
    let error = LexerError::InvalidCharacter('@', loc);

    assert_eq!(error.location(), loc);
    assert_eq!(
      error.to_string(),
      "Invalid character at 9999:9999 (Character: '@')"
    );
  }

  // Additional comprehensive tests for 100% coverage

  // Test all individual builder methods for ErrorRecoveryConfig
  #[test]
  fn test_error_recovery_config_individual_builder_methods() {
    let config = ErrorRecoveryConfig::new().continue_on_error(false);
    assert!(!config.continue_on_error);

    let config = ErrorRecoveryConfig::new().max_errors(Some(42));
    assert_eq!(config.max_errors, Some(42));

    let config = ErrorRecoveryConfig::new().insert_error_tokens(false);
    assert!(!config.insert_error_tokens);

    let config = ErrorRecoveryConfig::new().skip_invalid_chars(true);
    assert!(config.skip_invalid_chars);
  }

  // Test ErrorRecoveryConfig with None max_errors
  #[test]
  fn test_error_recovery_config_none_max_errors() {
    let config = ErrorRecoveryConfig::new().max_errors(None);
    assert_eq!(config.max_errors, None);
    assert!(config.validate().is_ok());
  }

  // Test ErrorRecoveryConfig with zero max_errors validation
  #[test]
  fn test_error_recovery_config_zero_max_errors_validation() {
    let config = ErrorRecoveryConfig {
      max_errors: Some(0),
      ..Default::default()
    };
    let validation_result = config.validate();
    assert!(validation_result.is_err());
    assert_eq!(validation_result.unwrap_err(), "max_errors cannot be 0");
  }

  // Test ErrorRecoveryConfig with continue_on_error false and max_errors validation
  #[test]
  fn test_error_recovery_config_continue_false_with_max_errors_validation() {
    let config = ErrorRecoveryConfig {
      continue_on_error: false,
      max_errors: Some(100),
      ..Default::default()
    };
    let validation_result = config.validate();
    assert!(validation_result.is_ok());
    // Note: When continue_on_error is false, max_errors is still meaningful
    // as a safety net to prevent infinite loops
  }

  // Test ErrorRecoveryConfig with continue_on_error false and max_errors None validation
  #[test]
  fn test_error_recovery_config_continue_false_without_max_errors_validation() {
    let config = ErrorRecoveryConfig {
      continue_on_error: false,
      max_errors: None,
      ..Default::default()
    };
    assert!(config.validate().is_ok());
  }

  // Test all LexerError variants with different characters and strings
  #[test]
  fn test_lexer_error_variants_with_different_content() {
    let loc = SourceLocation::new(1, 1, 0);

    // Test different characters for InvalidCharacter
    let error1 = LexerError::InvalidCharacter('$', loc);
    let error2 = LexerError::InvalidCharacter('\n', loc);
    let error3 = LexerError::InvalidCharacter('\t', loc);

    assert_eq!(error1.context(), Some("Character: '$'".to_string()));
    assert_eq!(error2.context(), Some("Character: '\n'".to_string()));
    assert_eq!(error3.context(), Some("Character: '\t'".to_string()));

    // Test different strings for InvalidEscapeSequence
    let error4 = LexerError::InvalidEscapeSequence("\\q".to_string(), loc);
    let error5 = LexerError::InvalidEscapeSequence("\\123".to_string(), loc);

    assert_eq!(error4.context(), Some("Sequence: '\\q'".to_string()));
    assert_eq!(error5.context(), Some("Sequence: '\\123'".to_string()));

    // Test different strings for InvalidNumber
    let error6 = LexerError::InvalidNumber("1.2.3".to_string(), loc);
    let error7 = LexerError::InvalidNumber("abc".to_string(), loc);

    assert_eq!(error6.context(), Some("Number: '1.2.3'".to_string()));
    assert_eq!(error7.context(), Some("Number: 'abc'".to_string()));

    // Test different strings for InvalidTokenSequence
    let error8 = LexerError::InvalidTokenSequence("+++".to_string(), loc);
    let error9 = LexerError::InvalidTokenSequence("---".to_string(), loc);

    assert_eq!(error8.context(), Some("Sequence: '+++'".to_string()));
    assert_eq!(error9.context(), Some("Sequence: '---'".to_string()));
  }

  // Test Display implementation with different SourceLocation formats
  #[test]
  fn test_lexer_error_display_different_locations() {
    let test_cases = vec![
      (SourceLocation::new(0, 0, 0), "0:0"),
      (SourceLocation::new(1, 1, 0), "1:1"),
      (SourceLocation::new(10, 20, 100), "10:20"),
      (SourceLocation::new(999, 999, 99999), "999:999"),
    ];

    for (loc, expected_format) in test_cases {
      let error = LexerError::InvalidCharacter('@', loc);
      let display_str = error.to_string();
      assert!(display_str.contains(expected_format));
      assert!(display_str.contains("Invalid character"));
      assert!(display_str.contains("Character: '@'"));
    }
  }

  // Test that all LexerError variants implement the required traits
  #[test]
  fn test_lexer_error_trait_implementations() {
    let loc = SourceLocation::new(1, 1, 0);

    // Test that all variants can be cloned
    let errors = vec![
      LexerError::InvalidCharacter('@', loc),
      LexerError::UnterminatedString(loc),
      LexerError::UnterminatedChar(loc),
      LexerError::UnterminatedBlockComment(loc),
      LexerError::UnterminatedComment(loc),
      LexerError::InvalidEscapeSequence("\\x".to_string(), loc),
      LexerError::InvalidNumber("12.34.56".to_string(), loc),
      LexerError::UnexpectedEndOfInput(loc),
      LexerError::InvalidTokenSequence("+++".to_string(), loc),
    ];

    for error in errors {
      let cloned = error.clone();
      assert_eq!(error, cloned);

      // Test Debug implementation
      let debug_str = format!("{:?}", error);
      assert!(!debug_str.is_empty());

      // Test Display implementation
      let display_str = error.to_string();
      assert!(!display_str.is_empty());

      // Test Error trait implementation
      let error_ref: &dyn std::error::Error = &error;
      let description = error_ref.description();
      assert!(!description.is_empty());
    }
  }

  // Test ErrorRecoveryConfig with extreme values
  #[test]
  fn test_error_recovery_config_extreme_values() {
    // Test with very large max_errors
    let config = ErrorRecoveryConfig::new().max_errors(Some(usize::MAX));
    assert_eq!(config.max_errors, Some(usize::MAX));
    assert!(config.validate().is_ok());

    // Test with very large max_errors when continue_on_error is false
    let config = ErrorRecoveryConfig {
      continue_on_error: false,
      max_errors: Some(usize::MAX),
      ..Default::default()
    };
    assert!(config.validate().is_ok());
  }

  // Test ErrorRecoveryConfig field access
  #[test]
  fn test_error_recovery_config_field_access() {
    let config = ErrorRecoveryConfig {
      continue_on_error: false,
      max_errors: Some(50),
      insert_error_tokens: false,
      skip_invalid_chars: true,
    };

    assert_eq!(config.continue_on_error, false);
    assert_eq!(config.max_errors, Some(50));
    assert_eq!(config.insert_error_tokens, false);
    assert_eq!(config.skip_invalid_chars, true);
  }

  // Test ErrorRecoveryConfig with all combinations of boolean fields
  #[test]
  fn test_error_recovery_config_boolean_combinations() {
    let combinations = vec![
      (true, true, true, true),
      (true, true, true, false),
      (true, true, false, true),
      (true, true, false, false),
      (true, false, true, true),
      (true, false, true, false),
      (true, false, false, true),
      (true, false, false, false),
      (false, true, true, true),
      (false, true, true, false),
      (false, true, false, true),
      (false, true, false, false),
      (false, false, true, true),
      (false, false, true, false),
      (false, false, false, true),
      (false, false, false, false),
    ];

    for (continue_on_error, insert_error_tokens, skip_invalid_chars, _) in combinations {
      let config = ErrorRecoveryConfig {
        continue_on_error,
        max_errors: Some(100),
        insert_error_tokens,
        skip_invalid_chars,
      };

      // Test that fields are set correctly
      assert_eq!(config.continue_on_error, continue_on_error);
      assert_eq!(config.insert_error_tokens, insert_error_tokens);
      assert_eq!(config.skip_invalid_chars, skip_invalid_chars);

      // Test that the config can be cloned and compared
      let cloned = config.clone();
      assert_eq!(config, cloned);

      // Test Debug implementation
      let debug_str = format!("{:?}", config);
      assert!(!debug_str.is_empty());
    }
  }

  // Test that all ErrorRecoveryConfig constructors work correctly
  #[test]
  fn test_error_recovery_config_all_constructors() {
    // Test default
    let default_config = ErrorRecoveryConfig::default();
    assert!(default_config.continue_on_error);
    assert_eq!(default_config.max_errors, Some(100));
    assert!(default_config.insert_error_tokens);
    assert!(!default_config.skip_invalid_chars);

    // Test new (should be same as default)
    let new_config = ErrorRecoveryConfig::new();
    assert_eq!(default_config, new_config);

    // Test strict
    let strict_config = ErrorRecoveryConfig::strict();
    assert!(!strict_config.continue_on_error);
    assert_eq!(strict_config.max_errors, None);
    assert!(!strict_config.insert_error_tokens);
    assert!(!strict_config.skip_invalid_chars);

    // Test permissive
    let permissive_config = ErrorRecoveryConfig::permissive();
    assert!(permissive_config.continue_on_error);
    assert_eq!(permissive_config.max_errors, None);
    assert!(permissive_config.insert_error_tokens);
    assert!(permissive_config.skip_invalid_chars);
  }

  // Test comprehensive validation scenarios
  #[test]
  fn test_error_recovery_config_comprehensive_validation() {
    // Valid configurations
    let valid_configs = vec![
      ErrorRecoveryConfig::default(),
      ErrorRecoveryConfig::strict(),
      ErrorRecoveryConfig::permissive(),
      ErrorRecoveryConfig::new().max_errors(Some(1)),
      ErrorRecoveryConfig::new().max_errors(Some(1000)),
      ErrorRecoveryConfig::new().max_errors(None),
      ErrorRecoveryConfig {
        continue_on_error: false,
        max_errors: None,
        ..Default::default()
      },
      ErrorRecoveryConfig {
        continue_on_error: false,
        max_errors: Some(1),
        ..Default::default()
      },
    ];

    for config in valid_configs {
      assert!(
        config.validate().is_ok(),
        "Config should be valid: {:?}",
        config
      );
    }

    // Invalid configurations
    let invalid_configs = vec![ErrorRecoveryConfig {
      max_errors: Some(0),
      ..Default::default()
    }];

    for config in invalid_configs {
      assert!(
        config.validate().is_err(),
        "Config should be invalid: {:?}",
        config
      );
    }
  }
}
