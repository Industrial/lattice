//! Main lexer implementation for the Lattice language.
//!
//! This module provides the core Lexer struct that integrates token definitions,
//! error handling, and source location tracking into a unified lexical analysis system.

use crate::lexer::errors::{ErrorRecoveryConfig, LexerError};
use crate::lexer::tokens::{SourceLocation, Token, TokenKind};
use crate::lexer::traits::{
  LexerBuilder as LexerBuilderTrait, LexerConfig as LexerConfigTrait, TokenStream,
};
use logos::Logos;
use std::fmt;
use std::iter::Iterator;

/// Configuration for the lexer behavior
#[derive(Debug, Clone, PartialEq)]
pub struct LexerConfig {
  /// Error recovery configuration
  pub error_recovery: ErrorRecoveryConfig,
  /// Whether to include comments in the token stream
  pub include_comments: bool,
  /// Whether to include whitespace in the token stream
  pub include_whitespace: bool,
}

impl Default for LexerConfig {
  fn default() -> Self {
    Self {
      error_recovery: ErrorRecoveryConfig::default(),
      include_comments: false,
      include_whitespace: false,
    }
  }
}

impl LexerConfig {
  /// Create a new lexer configuration
  pub fn new() -> Self {
    Self::default()
  }

  /// Create a strict configuration that stops on first error
  pub fn strict() -> Self {
    Self {
      error_recovery: ErrorRecoveryConfig::strict(),
      include_comments: false,
      include_whitespace: false,
    }
  }

  /// Create a permissive configuration that continues on errors
  pub fn permissive() -> Self {
    Self {
      error_recovery: ErrorRecoveryConfig::permissive(),
      include_comments: true,
      include_whitespace: false,
    }
  }

  /// Set the error recovery configuration
  pub fn with_error_recovery(mut self, config: ErrorRecoveryConfig) -> Self {
    self.error_recovery = config;
    self
  }

  /// Set whether to include comments
  pub fn with_comments(mut self, include: bool) -> Self {
    self.include_comments = include;
    self
  }

  /// Set whether to include whitespace
  pub fn with_whitespace(mut self, include: bool) -> Self {
    self.include_whitespace = include;
    self
  }
}

impl LexerConfigTrait for LexerConfig {
  fn error_recovery(&self) -> &ErrorRecoveryConfig {
    &self.error_recovery
  }

  fn include_comments(&self) -> bool {
    self.include_comments
  }

  fn include_whitespace(&self) -> bool {
    self.include_whitespace
  }

  fn validate(&self) -> Result<(), String> {
    self.error_recovery.validate()
  }
}

/// Builder for creating lexer instances with custom configuration
#[derive(Debug)]
pub struct LexerBuilder {
  config: LexerConfig,
}

impl LexerBuilder {
  /// Create a new lexer builder with default configuration
  pub fn new() -> Self {
    Self {
      config: LexerConfig::default(),
    }
  }

  /// Set the error recovery configuration
  pub fn with_error_recovery(mut self, config: ErrorRecoveryConfig) -> Self {
    self.config.error_recovery = config;
    self
  }

  /// Set whether to include comments
  pub fn with_comments(mut self, include: bool) -> Self {
    self.config.include_comments = include;
    self
  }

  /// Set whether to include whitespace
  pub fn with_whitespace(mut self, include: bool) -> Self {
    self.config.include_whitespace = include;
    self
  }

  /// Build the lexer with the current configuration
  pub fn build(self, source: String) -> Result<Lexer, String> {
    self.config.validate()?;
    Ok(Lexer::with_config(source, self.config))
  }
}

impl LexerBuilderTrait for LexerBuilder {
  fn with_error_recovery(self, config: ErrorRecoveryConfig) -> Self {
    self.with_error_recovery(config)
  }

  fn with_comments(self, include: bool) -> Self {
    self.with_comments(include)
  }

  fn with_whitespace(self, include: bool) -> Self {
    self.with_whitespace(include)
  }

  fn build(self) -> Result<Box<dyn TokenStream>, String> {
    // For now, we'll use a dummy source since we need a source string
    // In a real implementation, this would be handled differently
    Err("LexerBuilder::build() requires a source string".to_string())
  }
}

/// The main lexer struct that provides lexical analysis for Lattice source code
pub struct Lexer {
  /// The source code to be tokenized
  source: String,
  /// Current location in the source code
  current_location: SourceLocation,
  /// Configuration for the lexer behavior
  config: LexerConfig,
  /// Internal logos lexer for tokenization
  logos_lexer: Option<TokenKind>,
  /// Current position in the logos lexer
  logos_position: usize,
}

impl Lexer {
  /// Create a new lexer from a string slice
  pub fn from_str(source: &str) -> Self {
    Self {
      source: source.to_string(),
      current_location: SourceLocation::start(),
      config: LexerConfig::default(),
      logos_lexer: None,
      logos_position: 0,
    }
  }

  /// Create a new lexer with custom configuration
  pub fn with_config(source: String, config: LexerConfig) -> Self {
    Self {
      source,
      current_location: SourceLocation::start(),
      config,
      logos_lexer: None,
      logos_position: 0,
    }
  }

  /// Create a new lexer builder
  pub fn builder() -> LexerBuilder {
    LexerBuilder::new()
  }

  /// Get the current configuration
  pub fn config(&self) -> &LexerConfig {
    &self.config
  }

  /// Get the error recovery configuration
  pub fn error_recovery_config(&self) -> &ErrorRecoveryConfig {
    &self.config.error_recovery
  }

  /// Tokenize the source code, returning a vector of tokens
  ///
  /// This method filters out comments and whitespace by default,
  /// and stops on the first error encountered.
  pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    let mut logos_lexer = TokenKind::lexer(&self.source);
    let mut current_location = SourceLocation::start();

    while let Some(result) = logos_lexer.next() {
      match result {
        Ok(kind) => {
          let text = logos_lexer.slice().to_string();
          let start = current_location;

          // Advance location for the token text
          let mut end_location = start;
          end_location.advance_by(&text);

          let token = Token::new(kind, text, start, end_location);

          // Filter tokens based on configuration
          if self.should_include_token(&token) {
            tokens.push(token);
          }

          current_location = end_location;
        }
        Err(_) => {
          // Handle logos errors
          let error = LexerError::InvalidCharacter(
            self
              .source
              .chars()
              .nth(current_location.offset)
              .unwrap_or('\0'),
            current_location,
          );

          if !self.config.error_recovery.continue_on_error {
            return Err(error);
          }

          errors.push(error.clone());

          if let Some(max_errors) = self.config.error_recovery.max_errors {
            if errors.len() >= max_errors {
              break;
            }
          }

          // Try to recover and continue
          if let Some(recovery_pos) =
            self.try_recover_from_error(&error, &self.source, current_location.offset)
          {
            current_location.offset = recovery_pos;
            current_location.line = 1;
            current_location.column = 1;
            // Recalculate line/column based on offset
            for (_i, ch) in self.source[..recovery_pos].chars().enumerate() {
              if ch == '\n' {
                current_location.line += 1;
                current_location.column = 1;
              } else {
                current_location.column += 1;
              }
            }
          }
        }
      }
    }

    if !errors.is_empty() {
      Err(errors.remove(0)) // Return first error for backward compatibility
    } else {
      Ok(tokens)
    }
  }

  /// Tokenize the source code with error collection
  ///
  /// This method collects all errors encountered during tokenization
  /// and returns them along with any successfully tokenized tokens.
  pub fn tokenize_with_errors(&mut self) -> Result<Vec<Token>, Vec<LexerError>> {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    let mut logos_lexer = TokenKind::lexer(&self.source);
    let mut current_location = SourceLocation::start();

    while let Some(result) = logos_lexer.next() {
      match result {
        Ok(kind) => {
          let text = logos_lexer.slice().to_string();
          let start = current_location;

          // Advance location for the token text
          let mut end_location = start;
          end_location.advance_by(&text);

          let token = Token::new(kind, text, start, end_location);

          // Filter tokens based on configuration
          if self.should_include_token(&token) {
            tokens.push(token);
          }

          current_location = end_location;
        }
        Err(_) => {
          // Handle logos errors
          let error = LexerError::InvalidCharacter(
            self
              .source
              .chars()
              .nth(current_location.offset)
              .unwrap_or('\0'),
            current_location,
          );

          errors.push(error.clone());

          if let Some(max_errors) = self.config.error_recovery.max_errors {
            if errors.len() >= max_errors {
              break;
            }
          }

          // Try to recover and continue
          if let Some(recovery_pos) =
            self.try_recover_from_error(&error, &self.source, current_location.offset)
          {
            current_location.offset = recovery_pos;
            current_location.line = 1;
            current_location.column = 1;
            // Recalculate line/column based on offset
            for (_i, ch) in self.source[..recovery_pos].chars().enumerate() {
              if ch == '\n' {
                current_location.line += 1;
                current_location.column = 1;
              } else {
                current_location.column += 1;
              }
            }
          }
        }
      }
    }

    if errors.is_empty() {
      Ok(tokens)
    } else {
      Err(errors)
    }
  }

  /// Check if a token should be included based on the configuration
  fn should_include_token(&self, token: &Token) -> bool {
    match token.kind {
      TokenKind::LineComment
      | TokenKind::BlockComment
      | TokenKind::DocComment
      | TokenKind::DocBlockComment => self.config.include_comments,
      TokenKind::Whitespace => self.config.include_whitespace,
      _ => true,
    }
  }

  /// Attempt to recover from a specific error type
  pub fn try_recover_from_error(
    &self,
    error: &LexerError,
    source: &str,
    position: usize,
  ) -> Option<usize> {
    match error {
      LexerError::UnterminatedString(_) => {
        // Try to find the next quote character
        if let Some(next_quote) = source[position..].find('"') {
          Some(position + next_quote + 1)
        } else {
          None
        }
      }
      LexerError::UnterminatedChar(_) => {
        // Try to find the next single quote character
        if let Some(next_quote) = source[position..].find('\'') {
          Some(position + next_quote + 1)
        } else {
          None
        }
      }
      LexerError::UnterminatedBlockComment(_) => {
        // Try to find the end of block comment
        if let Some(end_comment) = source[position..].find("*/") {
          Some(position + end_comment + 2)
        } else {
          Some(source.len())
        }
      }
      LexerError::UnterminatedComment(_) => {
        // Line comments end at newline
        if let Some(newline) = source[position..].find('\n') {
          Some(position + newline + 1)
        } else {
          Some(source.len())
        }
      }
      _ => {
        // For other errors, try to skip to the next whitespace or delimiter
        let next_whitespace = source[position..].find(|c: char| c.is_whitespace());
        let next_delimiter = source[position..]
          .find(|c: char| matches!(c, '(' | ')' | '[' | ']' | '{' | '}' | ',' | ';' | ':'));

        match (next_whitespace, next_delimiter) {
          (Some(ws), Some(del)) => Some(position + ws.min(del)),
          (Some(ws), None) => Some(position + ws),
          (None, Some(del)) => Some(position + del),
          (None, None) => Some(source.len()),
        }
      }
    }
  }

  /// Get a summary of all errors encountered during lexing
  pub fn get_error_summary(&self, errors: &[LexerError]) -> String {
    if errors.is_empty() {
      return "No errors encountered".to_string();
    }

    let mut summary = format!("{} error(s) encountered:\n", errors.len());
    for (i, error) in errors.iter().enumerate() {
      summary.push_str(&format!("  {}. {}\n", i + 1, error));
    }
    summary
  }

  /// Reset the lexer to the beginning of the source
  pub fn reset(&mut self) {
    self.current_location = SourceLocation::start();
    self.logos_lexer = None;
    self.logos_position = 0;
  }

  /// Peek at the next token without consuming it
  pub fn peek(&self) -> Option<TokenKind> {
    let mut lexer = TokenKind::lexer(&self.source);
    lexer.next().transpose().ok().flatten()
  }

  /// Check if the lexer has reached the end
  pub fn is_eof(&self) -> bool {
    self.current_location.offset >= self.source.len()
  }

  /// Get the remaining source code
  pub fn remaining(&self) -> &str {
    if self.current_location.offset >= self.source.len() {
      ""
    } else {
      &self.source[self.current_location.offset..]
    }
  }

  /// Validate that the lexer configuration is reasonable
  pub fn validate_config(&self) -> Result<(), String> {
    self.config.validate()
  }
}

impl Iterator for Lexer {
  type Item = Result<Token, LexerError>;

  fn next(&mut self) -> Option<Self::Item> {
    // For now, we'll use a simple approach
    // In a real implementation, this would maintain state between calls
    if self.is_eof() {
      return None;
    }

    // This is a simplified implementation
    // The real implementation would maintain state and return tokens one by one
    None
  }
}

impl TokenStream for Lexer {
  fn peek(&self) -> Option<&Token> {
    // This would need to be implemented with proper state management
    None
  }

  fn position(&self) -> usize {
    self.current_location.offset
  }

  fn is_eof(&self) -> bool {
    self.is_eof()
  }

  fn remaining(&self) -> Vec<Token> {
    // This would need to be implemented with proper state management
    Vec::new()
  }

  fn reset(&mut self) {
    self.reset();
  }
}

impl From<&str> for Lexer {
  fn from(source: &str) -> Self {
    Self::from_str(source)
  }
}

impl fmt::Display for Lexer {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "Lexer(source_length: {}, config: {:?})",
      self.source.len(),
      self.config
    )
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_lexer_creation() {
    let lexer = Lexer::from_str("let x = 42");
    assert_eq!(lexer.source, "let x = 42");
    assert_eq!(lexer.current_location, SourceLocation::start());
  }

  #[test]
  fn test_lexer_with_config() {
    let config = LexerConfig::strict();
    let lexer = Lexer::with_config("test".to_string(), config);
    assert_eq!(lexer.config.error_recovery.continue_on_error, false);
  }

  #[test]
  fn test_lexer_builder() {
    let lexer = Lexer::builder()
      .with_comments(true)
      .with_whitespace(false)
      .build("test".to_string())
      .unwrap();

    assert!(lexer.config.include_comments);
    assert!(!lexer.config.include_whitespace);
  }

  #[test]
  fn test_lexer_config_default() {
    let config = LexerConfig::default();
    assert!(config.error_recovery.continue_on_error);
    assert!(!config.include_comments);
    assert!(!config.include_whitespace);
  }

  #[test]
  fn test_lexer_config_strict() {
    let config = LexerConfig::strict();
    assert!(!config.error_recovery.continue_on_error);
    assert_eq!(config.error_recovery.max_errors, Some(1));
  }

  #[test]
  fn test_lexer_config_permissive() {
    let config = LexerConfig::permissive();
    assert!(config.error_recovery.continue_on_error);
    assert_eq!(config.error_recovery.max_errors, None);
    assert!(config.include_comments);
  }

  #[test]
  fn test_lexer_basic_tokenization() {
    let mut lexer = Lexer::from_str("let x = 42");
    let result = lexer.tokenize();
    assert!(result.is_ok());

    let tokens = result.unwrap();
    assert!(!tokens.is_empty());
  }

  #[test]
  fn test_lexer_error_recovery() {
    let mut lexer = Lexer::from_str("let x = \u{0000} y = 42");
    let result = lexer.tokenize_with_errors();

    match result {
      Err(errors) => {
        assert!(!errors.is_empty());
        let summary = lexer.get_error_summary(&errors);
        assert!(!summary.is_empty());
      }
      Ok(_) => panic!("Expected errors for invalid input"),
    }
  }

  #[test]
  fn test_lexer_reset() {
    let mut lexer = Lexer::from_str("let x = 42");
    let initial_location = lexer.current_location;

    // Simulate some processing
    lexer.current_location.advance('l');

    // Reset
    lexer.reset();
    assert_eq!(lexer.current_location, initial_location);
  }

  #[test]
  fn test_lexer_remaining() {
    let mut lexer = Lexer::from_str("let x = 42");
    assert_eq!(lexer.remaining(), "let x = 42");

    // Simulate processing
    lexer.current_location.advance_by("let ");
    assert_eq!(lexer.remaining(), "x = 42");
  }

  #[test]
  fn test_lexer_config_validation() {
    let config = LexerConfig::default();
    assert!(config.validate().is_ok());

    let invalid_config = LexerConfig {
      error_recovery: ErrorRecoveryConfig {
        max_errors: Some(0),
        ..Default::default()
      },
      ..Default::default()
    };
    assert!(invalid_config.validate().is_err());
  }
}
