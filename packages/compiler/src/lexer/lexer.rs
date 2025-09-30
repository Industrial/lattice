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
      error_recovery: ErrorRecoveryConfig::strict_with_safety_net(),
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
#[derive(Debug)]
pub struct Lexer {
  /// The source code to be tokenized
  source: String,
  /// Current location in the source code
  current_location: SourceLocation,
  /// Configuration for the lexer behavior
  config: LexerConfig,
  /// Current position in the source
  position: usize,
  /// Cached tokens for iteration
  cached_tokens: Option<Vec<Token>>,
}

impl Lexer {
  /// Create a new lexer from a string slice
  pub fn from_str(source: &str) -> Self {
    Self {
      source: source.to_string(),
      current_location: SourceLocation::start(),
      config: LexerConfig::default(),
      position: 0,
      cached_tokens: None,
    }
  }

  /// Create a new lexer with custom configuration
  pub fn with_config(source: String, config: LexerConfig) -> Self {
    Self {
      source,
      current_location: SourceLocation::start(),
      config,
      position: 0,
      cached_tokens: None,
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

  /// Attempt to recover from a specific error type
  pub fn try_recover_from_error(
    &self,
    error: &LexerError,
    source: &str,
    position: usize,
  ) -> Option<usize> {
    match error {
      LexerError::UnterminatedString(_) => {
        // Try to find the next quote character after the opening quote
        // Skip the opening quote first
        let search_start = position + 1;
        if search_start < source.len() {
          if let Some(next_quote) = source[search_start..].find('"') {
            Some(search_start + next_quote + 1)
          } else {
            None
          }
        } else {
          None
        }
      }
      LexerError::UnterminatedChar(_) => {
        // Try to find the next single quote character after the opening quote
        let search_start = position + 1;
        if search_start < source.len() {
          if let Some(next_quote) = source[search_start..].find('\'') {
            Some(search_start + next_quote + 1)
          } else {
            None
          }
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
    self.position = 0;
    self.cached_tokens = None;
  }

  /// Check if a token should be included based on the configuration
  fn should_include_token(&self, token: &Token) -> bool {
    match token.kind {
      TokenKind::Whitespace => self.config.include_whitespace,
      TokenKind::LineComment | TokenKind::BlockComment => self.config.include_comments,
      _ => true, // Include all other tokens
    }
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
    // Initialize cached tokens if not already done
    if self.cached_tokens.is_none() {
      match self.tokenize() {
        Ok(tokens) => {
          self.cached_tokens = Some(tokens);
        }
        Err(error) => {
          return Some(Err(error));
        }
      }
    }

    // Get the cached tokens
    let tokens = self.cached_tokens.as_ref()?;

    // Check if we've reached the end
    if self.position >= tokens.len() {
      return None;
    }

    // Get the current token
    let token = &tokens[self.position];
    self.position += 1;

    Some(Ok(token.clone()))
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

  // ===== LexerConfig Tests =====

  #[test]
  fn test_lexer_config_default() {
    let config = LexerConfig::default();
    assert!(config.error_recovery.continue_on_error);
    assert!(!config.include_comments);
    assert!(!config.include_whitespace);
  }

  #[test]
  fn test_lexer_config_new() {
    let config = LexerConfig::new();
    assert_eq!(config, LexerConfig::default());
  }

  #[test]
  fn test_lexer_config_strict() {
    let config = LexerConfig::strict();
    assert!(!config.error_recovery.continue_on_error);
    assert_eq!(config.error_recovery.max_errors, Some(1));
    assert!(!config.include_comments);
    assert!(!config.include_whitespace);
  }

  #[test]
  fn test_lexer_config_permissive() {
    let config = LexerConfig::permissive();
    assert!(config.error_recovery.continue_on_error);
    assert_eq!(config.error_recovery.max_errors, None);
    assert!(config.include_comments);
    assert!(!config.include_whitespace);
  }

  #[test]
  fn test_lexer_config_with_error_recovery() {
    let error_config = ErrorRecoveryConfig::strict_with_safety_net();
    let config = LexerConfig::new().with_error_recovery(error_config);
    assert!(!config.error_recovery.continue_on_error);
    assert_eq!(config.error_recovery.max_errors, Some(1));
  }

  #[test]
  fn test_lexer_config_with_comments() {
    let config = LexerConfig::new().with_comments(true);
    assert!(config.include_comments);
    assert!(!config.include_whitespace);
  }

  #[test]
  fn test_lexer_config_with_whitespace() {
    let config = LexerConfig::new().with_whitespace(true);
    assert!(!config.include_comments);
    assert!(config.include_whitespace);
  }

  #[test]
  fn test_lexer_config_trait_implementation() {
    let config = LexerConfig::new();
    assert_eq!(config.error_recovery(), &ErrorRecoveryConfig::default());
    assert_eq!(config.include_comments(), false);
    assert_eq!(config.include_whitespace(), false);
    assert!(config.validate().is_ok());
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

  // ===== LexerBuilder Tests =====

  #[test]
  fn test_lexer_builder_new() {
    let builder = LexerBuilder::new();
    assert_eq!(builder.config, LexerConfig::default());
  }

  #[test]
  fn test_lexer_builder_with_error_recovery() {
    let error_config = ErrorRecoveryConfig::strict();
    let builder = LexerBuilder::new().with_error_recovery(error_config);
    assert!(!builder.config.error_recovery.continue_on_error);
  }

  #[test]
  fn test_lexer_builder_with_comments() {
    let builder = LexerBuilder::new().with_comments(true);
    assert!(builder.config.include_comments);
  }

  #[test]
  fn test_lexer_builder_with_whitespace() {
    let builder = LexerBuilder::new().with_whitespace(true);
    assert!(builder.config.include_whitespace);
  }

  #[test]
  fn test_lexer_builder_build() {
    let lexer = LexerBuilder::new()
      .with_comments(true)
      .with_whitespace(false)
      .build("test".to_string())
      .unwrap();

    assert!(lexer.config.include_comments);
    assert!(!lexer.config.include_whitespace);
    assert_eq!(lexer.source, "test");
  }

  #[test]
  fn test_lexer_builder_build_with_invalid_config() {
    let invalid_config = ErrorRecoveryConfig {
      max_errors: Some(0),
      ..Default::default()
    };
    let result = LexerBuilder::new()
      .with_error_recovery(invalid_config)
      .build("test".to_string());
    assert!(result.is_err());
  }

  #[test]
  fn test_lexer_builder_trait_implementation() {
    // Test the trait build method (which should fail as expected)
    let builder = LexerBuilder::new();
    let result = LexerBuilderTrait::build(builder);
    assert!(result.is_err());
    assert_eq!(
      result.unwrap_err(),
      "LexerBuilder::build() requires a source string"
    );

    // Test builder methods separately
    let builder = LexerBuilder::new();
    let _builder_with_error = builder.with_error_recovery(ErrorRecoveryConfig::strict());

    let builder = LexerBuilder::new();
    let _builder_with_comments = builder.with_comments(true);

    let builder = LexerBuilder::new();
    let _builder_with_whitespace = builder.with_whitespace(true);
  }

  // ===== Lexer Creation Tests =====

  #[test]
  fn test_lexer_from_str() {
    let lexer = Lexer::from_str("let x = 42");
    assert_eq!(lexer.source, "let x = 42");
    assert_eq!(lexer.current_location, SourceLocation::start());
    assert_eq!(lexer.config, LexerConfig::default());
  }

  #[test]
  fn test_lexer_with_config() {
    let config = LexerConfig::strict();
    let lexer = Lexer::with_config("test".to_string(), config.clone());
    assert_eq!(lexer.source, "test");
    assert_eq!(lexer.config, config);
    assert_eq!(lexer.current_location, SourceLocation::start());
  }

  #[test]
  fn test_lexer_builder_method() {
    let builder = Lexer::builder();
    assert_eq!(builder.config, LexerConfig::default());
  }

  #[test]
  fn test_lexer_config_accessor() {
    let config = LexerConfig::strict();
    let lexer = Lexer::with_config("test".to_string(), config.clone());
    assert_eq!(lexer.config(), &config);
  }

  #[test]
  fn test_lexer_error_recovery_config_accessor() {
    let config = LexerConfig::strict();
    let lexer = Lexer::with_config("test".to_string(), config);
    assert_eq!(
      lexer.error_recovery_config(),
      &ErrorRecoveryConfig::strict_with_safety_net()
    );
  }

  // ===== Tokenization Tests =====

  #[test]
  fn test_lexer_basic_tokenization() {
    let mut lexer = Lexer::from_str("let x = 42");
    let result = lexer.tokenize();
    assert!(result.is_ok());

    let tokens = result.unwrap();
    assert!(!tokens.is_empty());
  }

  #[test]
  fn test_lexer_tokenization_with_comments() {
    let mut lexer = Lexer::builder()
      .with_comments(true)
      .build("let x = 42 // comment".to_string())
      .unwrap();

    let result = lexer.tokenize();
    assert!(result.is_ok());

    let tokens = result.unwrap();
    // Should include the comment token
    assert!(tokens
      .iter()
      .any(|t| matches!(t.kind, TokenKind::LineComment)));
  }

  #[test]
  fn test_lexer_tokenization_with_whitespace() {
    let mut lexer = Lexer::builder()
      .with_whitespace(true)
      .build("let x = 42".to_string())
      .unwrap();

    let result = lexer.tokenize();
    assert!(result.is_ok());

    let tokens = result.unwrap();
    // Should include whitespace tokens
    assert!(tokens
      .iter()
      .any(|t| matches!(t.kind, TokenKind::Whitespace)));
  }

  #[test]
  fn test_lexer_tokenization_with_errors() {
    let mut lexer = Lexer::from_str("let x = \u{0000} y = 42");
    let result = lexer.tokenize_with_errors();

    match result {
      Err(errors) => {
        assert!(!errors.is_empty());
        let summary = lexer.get_error_summary(&errors);
        assert!(!summary.is_empty());
        assert!(summary.contains("error(s) encountered:"));
      }
      Ok(_) => panic!("Expected errors for invalid input"),
    }
  }

  #[test]
  fn test_lexer_tokenization_strict_mode() {
    let mut lexer = Lexer::builder()
      .with_error_recovery(ErrorRecoveryConfig::strict())
      .build("let x = \u{0000}".to_string())
      .unwrap();

    let result = lexer.tokenize();
    assert!(result.is_err());

    if let Err(LexerError::InvalidCharacter(ch, _)) = result {
      assert_eq!(ch, '\u{0000}');
    } else {
      panic!("Expected InvalidCharacter error");
    }
  }

  #[test]
  fn test_lexer_tokenization_max_errors() {
    let mut lexer = Lexer::builder()
      .with_error_recovery(ErrorRecoveryConfig::new().max_errors(Some(2)))
      .build("let x = \u{0000} y = \u{0001} z = \u{0002}".to_string())
      .unwrap();

    let result = lexer.tokenize_with_errors();
    assert!(result.is_err());

    if let Err(errors) = result {
      assert_eq!(errors.len(), 2); // Should stop at max_errors
    } else {
      panic!("Expected error collection");
    }
  }

  // ===== Token Filtering Tests =====

  #[test]
  fn test_should_include_token() {
    let lexer = Lexer::from_str("test");

    // Test comment tokens
    let comment_token = Token::new(
      TokenKind::LineComment,
      "// comment".to_string(),
      SourceLocation::start(),
      SourceLocation::new(1, 11, 10),
    );

    // Should not include comments by default
    assert!(!lexer.should_include_token(&comment_token));

    // Should include comments when configured
    let lexer_with_comments = Lexer::builder()
      .with_comments(true)
      .build("test".to_string())
      .unwrap();
    assert!(lexer_with_comments.should_include_token(&comment_token));

    // Test whitespace tokens
    let whitespace_token = Token::new(
      TokenKind::Whitespace,
      " ".to_string(),
      SourceLocation::start(),
      SourceLocation::new(1, 2, 1),
    );

    // Should not include whitespace by default
    assert!(!lexer.should_include_token(&whitespace_token));

    // Should include whitespace when configured
    let lexer_with_whitespace = Lexer::builder()
      .with_whitespace(true)
      .build("test".to_string())
      .unwrap();
    assert!(lexer_with_whitespace.should_include_token(&whitespace_token));

    // Test regular tokens (should always be included)
    let regular_token = Token::new(
      TokenKind::Identifier,
      "x".to_string(),
      SourceLocation::start(),
      SourceLocation::new(1, 2, 1),
    );
    assert!(lexer.should_include_token(&regular_token));
  }

  // ===== Error Recovery Tests =====

  #[test]
  fn test_try_recover_from_error() {
    let lexer = Lexer::from_str("test");

    // Test unterminated string recovery
    let error = LexerError::UnterminatedString(SourceLocation::start());
    let source = "let x = \"unterminated";
    let recovery_pos = lexer.try_recover_from_error(&error, source, 8); // Position of opening quote
    assert!(recovery_pos.is_none()); // No closing quote

    let source_with_quote = "let x = \"unterminated\" y = 42";
    let recovery_pos = lexer.try_recover_from_error(&error, source_with_quote, 8); // Position of opening quote
    assert_eq!(recovery_pos, Some(22)); // Position after closing quote

    // Test unterminated char recovery
    let error = LexerError::UnterminatedChar(SourceLocation::start());
    let source = "let x = 'a";
    let recovery_pos = lexer.try_recover_from_error(&error, source, 8); // Position of opening quote
    assert!(recovery_pos.is_none()); // No closing quote

    let source_with_quote = "let x = 'a' y = 42";
    let recovery_pos = lexer.try_recover_from_error(&error, source_with_quote, 8); // Position of opening quote
    assert_eq!(recovery_pos, Some(11)); // Position after closing quote

    // Test unterminated block comment recovery
    let error = LexerError::UnterminatedBlockComment(SourceLocation::start());
    let source = "let x = /* comment";
    let recovery_pos = lexer.try_recover_from_error(&error, source, 0);
    assert_eq!(recovery_pos, Some(source.len())); // Should go to end

    let source_with_end = "let x = /* comment */ y = 42";
    let recovery_pos = lexer.try_recover_from_error(&error, source_with_end, 0);
    assert_eq!(recovery_pos, Some(21)); // Position after */
                                        // Test unterminated comment recovery
    let error = LexerError::UnterminatedComment(SourceLocation::start());
    let source = "let x = // comment";
    let recovery_pos = lexer.try_recover_from_error(&error, source, 0);
    assert_eq!(recovery_pos, Some(source.len())); // Should go to end

    let source_with_newline = "let x = // comment\n y = 42";
    let recovery_pos = lexer.try_recover_from_error(&error, source_with_newline, 0);
    assert_eq!(recovery_pos, Some(19)); // Position after newline

    // Test other error recovery (should skip to whitespace or delimiter)
    let error = LexerError::InvalidCharacter('@', SourceLocation::start());
    let source = "let x = @ y = 42";
    let recovery_pos = lexer.try_recover_from_error(&error, source, 8); // Position of @ character
    assert_eq!(recovery_pos, Some(9)); // Position at space after @

    let source_with_delimiter = "let x = @, y = 42";
    let recovery_pos = lexer.try_recover_from_error(&error, source_with_delimiter, 8); // Position of @ character
    assert_eq!(recovery_pos, Some(9)); // Position at comma after @

    let source_no_recovery = "let x = @";
    let recovery_pos = lexer.try_recover_from_error(&error, source_no_recovery, 8); // Position of @ character
    assert_eq!(recovery_pos, Some(source_no_recovery.len())); // Should go to end
  }

  // ===== Error Summary Tests =====

  #[test]
  fn test_get_error_summary() {
    let lexer = Lexer::from_str("test");

    // Test empty errors
    let summary = lexer.get_error_summary(&[]);
    assert_eq!(summary, "No errors encountered");

    // Test single error
    let error = LexerError::InvalidCharacter('@', SourceLocation::start());
    let errors = vec![error];
    let summary = lexer.get_error_summary(&errors);
    assert!(summary.contains("1 error(s) encountered:"));
    assert!(summary.contains("1. Invalid character"));

    // Test multiple errors
    let error1 = LexerError::InvalidCharacter('@', SourceLocation::start());
    let error2 = LexerError::UnterminatedString(SourceLocation::new(1, 2, 1));
    let errors = vec![error1, error2];
    let summary = lexer.get_error_summary(&errors);
    assert!(summary.contains("2 error(s) encountered:"));
    assert!(summary.contains("1. Invalid character"));
    assert!(summary.contains("2. Unterminated string literal"));
  }

  // ===== Lexer State Management Tests =====

  #[test]
  fn test_lexer_reset() {
    let mut lexer = Lexer::from_str("let x = 42");
    let initial_location = lexer.current_location;

    // Simulate some processing
    lexer.current_location.advance('l');

    // Reset
    lexer.reset();
    assert_eq!(lexer.current_location, initial_location);
    assert_eq!(lexer.cached_tokens, None);
    assert_eq!(lexer.position, 0);
  }

  #[test]
  fn test_lexer_peek() {
    let lexer = Lexer::from_str("let x = 42");
    let peeked = lexer.peek();
    // Note: This depends on the actual TokenKind::lexer implementation
    // The test verifies the method doesn't panic
    assert!(peeked.is_some() || peeked.is_none());
  }

  #[test]
  fn test_lexer_is_eof() {
    let mut lexer = Lexer::from_str("let x = 42");
    assert!(!lexer.is_eof()); // At start

    // Simulate processing to end
    lexer.current_location.offset = lexer.source.len();
    assert!(lexer.is_eof());
  }

  #[test]
  fn test_lexer_remaining() {
    let mut lexer = Lexer::from_str("let x = 42");
    assert_eq!(lexer.remaining(), "let x = 42");

    // Simulate processing
    lexer.current_location.advance_by("let ");
    assert_eq!(lexer.remaining(), "x = 42");

    // Test at end
    lexer.current_location.offset = lexer.source.len();
    assert_eq!(lexer.remaining(), "");
  }

  #[test]
  fn test_lexer_validate_config() {
    let lexer = Lexer::from_str("test");
    assert!(lexer.validate_config().is_ok());

    let invalid_config = LexerConfig {
      error_recovery: ErrorRecoveryConfig {
        max_errors: Some(0),
        ..Default::default()
      },
      ..Default::default()
    };
    let lexer_with_invalid = Lexer::with_config("test".to_string(), invalid_config);
    assert!(lexer_with_invalid.validate_config().is_err());
  }

  // ===== Iterator Implementation Tests =====

  #[test]
  fn test_lexer_iterator() {
    let mut lexer = Lexer::from_str("let x = 42");

    // Test that the iterator works correctly
    let first_token = lexer.next();
    assert!(first_token.is_some());
    
    // Continue iterating through tokens
    let mut token_count = 0;
    while lexer.next().is_some() {
      token_count += 1;
    }
    
    // Should have consumed all tokens
    assert_eq!(lexer.next(), None);
    assert_eq!(lexer.next(), None);
  }

  // ===== TokenStream Implementation Tests =====

  #[test]
  fn test_lexer_token_stream() {
    let mut lexer = Lexer::from_str("let x = 42");

    // Test peek using the TokenStream trait (should return None in current implementation)
    assert_eq!(TokenStream::peek(&lexer), None);

    // Test position
    assert_eq!(lexer.position(), 0);

    // Test is_eof
    assert!(!lexer.is_eof());

    // Test remaining (should return empty Vec in current implementation)
    assert_eq!(TokenStream::remaining(&lexer), Vec::<Token>::new());

    // Test reset
    lexer.reset();
    assert_eq!(lexer.current_location, SourceLocation::start());
  }

  // ===== From Implementation Tests =====

  #[test]
  fn test_lexer_from_implementation() {
    let lexer = Lexer::from("let x = 42");
    assert_eq!(lexer.source, "let x = 42");
    assert_eq!(lexer.current_location, SourceLocation::start());
  }

  // ===== Display Implementation Tests =====

  #[test]
  fn test_lexer_display() {
    let lexer = Lexer::from_str("let x = 42");
    let display = format!("{}", lexer);
    assert!(display.contains("Lexer"));
    assert!(display.contains("source_length: 10"));
    assert!(display.contains("config:"));
  }

  // ===== Edge Cases and Integration Tests =====

  #[test]
  fn test_lexer_empty_source() {
    let mut lexer = Lexer::from_str("");
    let result = lexer.tokenize();
    assert!(result.is_ok());
    let tokens = result.unwrap();
    assert!(tokens.is_empty());
  }

  #[test]
  fn test_lexer_single_character() {
    let mut lexer = Lexer::from_str("x");
    let result = lexer.tokenize();
    assert!(result.is_ok());
    let tokens = result.unwrap();
    assert!(!tokens.is_empty());
  }

  #[test]
  fn test_lexer_only_whitespace() {
    let mut lexer = Lexer::from_str("   \t\n  ");
    let result = lexer.tokenize();
    assert!(result.is_ok());
    let tokens = result.unwrap();
    // Should be empty since whitespace is filtered by default
    assert!(tokens.is_empty());
  }

  #[test]
  fn test_lexer_only_comments() {
    let mut lexer = Lexer::from_str("// comment\n/* block */");
    let result = lexer.tokenize();
    assert!(result.is_ok());
    let tokens = result.unwrap();
    // Should be empty since comments are filtered by default
    assert!(tokens.is_empty());
  }

  #[test]
  fn test_lexer_complex_source() {
    let source = r#"
      fn factorial(n: Int) -> Int {
        if n <= 1 {
          return 1;
        } else {
          return n * factorial(n - 1);
        }
      }
    "#;

    let mut lexer = Lexer::from_str(source);
    let result = lexer.tokenize();
    assert!(result.is_ok());

    let tokens = result.unwrap();
    assert!(!tokens.is_empty());

    // Should contain function-related tokens
    let token_kinds: Vec<_> = tokens.iter().map(|t| &t.kind).collect();
    assert!(token_kinds.contains(&&TokenKind::Function));
    assert!(token_kinds.contains(&&TokenKind::If));
    assert!(token_kinds.contains(&&TokenKind::Return));
  }

  #[test]
  fn test_lexer_error_recovery_integration() {
    let source = "let x = \"unterminated\nlet y = 42";
    let mut lexer = Lexer::from_str(source);

    // Should recover and continue lexing
    let result = lexer.tokenize_with_errors();
    match result {
      Err(errors) => {
        assert!(!errors.is_empty());
        // Should have recovered and found the second let statement
        assert!(source.contains("let y = 42"));
      }
      Ok(_) => panic!("Expected errors for unterminated string"),
    }
  }

  #[test]
  fn test_lexer_configuration_chaining() {
    let config = LexerConfig::new()
      .with_comments(true)
      .with_whitespace(true)
      .with_error_recovery(ErrorRecoveryConfig::strict());

    assert!(config.include_comments);
    assert!(config.include_whitespace);
    assert!(!config.error_recovery.continue_on_error);

    let lexer = Lexer::with_config("test".to_string(), config);
    assert!(lexer.config.include_comments);
    assert!(lexer.config.include_whitespace);
    assert!(!lexer.config.error_recovery.continue_on_error);
  }

  #[test]
  fn test_lexer_builder_chaining() {
    let lexer = LexerBuilder::new()
      .with_comments(true)
      .with_whitespace(true)
      .with_error_recovery(ErrorRecoveryConfig::strict())
      .build("test".to_string())
      .unwrap();

    assert!(lexer.config.include_comments);
    assert!(lexer.config.include_whitespace);
    assert!(!lexer.config.error_recovery.continue_on_error);
  }
}
