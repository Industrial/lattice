//! Core traits and interfaces for the Lattice language lexer.
//!
//! This module defines the fundamental traits that the lexer implements,
//! providing a clean abstraction layer for tokenization and error handling.

use crate::lexer::errors::LexerError;
use crate::lexer::tokens::Token;
use std::iter::Iterator;

/// A stream of tokens that can be consumed by the parser
///
/// This trait provides a clean interface for consuming tokens from the lexer,
/// allowing the parser to work with any token source that implements this trait.
pub trait TokenStream: Iterator<Item = Result<Token, LexerError>> + std::fmt::Debug {
  /// Peek at the next token without consuming it
  fn peek(&self) -> Option<&Token>;

  /// Get the current position in the token stream
  fn position(&self) -> usize;

  /// Check if the token stream has reached the end
  fn is_eof(&self) -> bool;

  /// Get the remaining tokens in the stream
  fn remaining(&self) -> Vec<Token>;

  /// Reset the token stream to the beginning
  fn reset(&mut self);
}

/// Handler for lexical errors during tokenization
///
/// This trait allows custom error handling strategies to be implemented,
/// providing flexibility in how errors are processed and reported.
pub trait ErrorHandler {
  /// Handle a lexical error
  ///
  /// Returns `true` if the error was handled and lexing should continue,
  /// or `false` if lexing should stop.
  fn handle_error(&mut self, error: &LexerError) -> bool;

  /// Get all errors that have been collected
  fn get_errors(&self) -> &[LexerError];

  /// Clear all collected errors
  fn clear_errors(&mut self);

  /// Check if any errors have been collected
  fn has_errors(&self) -> bool;

  /// Get the number of errors collected
  fn error_count(&self) -> usize;
}

/// Configuration for the lexer behavior
///
/// This trait allows different lexer implementations to share common
/// configuration options while maintaining flexibility.
pub trait LexerConfig {
  /// Get the error recovery configuration
  fn error_recovery(&self) -> &crate::lexer::errors::ErrorRecoveryConfig;

  /// Get whether comments should be included in the token stream
  fn include_comments(&self) -> bool;

  /// Get whether whitespace should be included in the token stream
  fn include_whitespace(&self) -> bool;

  /// Validate the configuration
  fn validate(&self) -> Result<(), String>;
}

/// Builder pattern for creating lexer instances
///
/// This trait provides a fluent interface for configuring and creating
/// lexer instances with custom settings.
pub trait LexerBuilder {
  /// Set the error recovery configuration
  fn with_error_recovery(self, config: crate::lexer::errors::ErrorRecoveryConfig) -> Self;

  /// Set whether to include comments
  fn with_comments(self, include: bool) -> Self;

  /// Set whether to include whitespace
  fn with_whitespace(self, include: bool) -> Self;

  /// Build the lexer with the current configuration
  fn build(self) -> Result<Box<dyn TokenStream>, String>;
}

/// Iterator adapter for filtering tokens based on configuration
///
/// This struct wraps a token iterator and applies filtering based on
/// the lexer configuration (e.g., skipping comments and whitespace).
pub struct TokenFilter<I> {
  inner: I,
  include_comments: bool,
  include_whitespace: bool,
}

impl<I> TokenFilter<I>
where
  I: Iterator<Item = Result<Token, LexerError>>,
{
  /// Create a new token filter
  pub fn new(inner: I, include_comments: bool, include_whitespace: bool) -> Self {
    Self {
      inner,
      include_comments,
      include_whitespace,
    }
  }

  /// Check if a token should be included based on the configuration
  fn should_include(&self, token: &Token) -> bool {
    use crate::lexer::tokens::TokenKind;

    match token.kind {
      TokenKind::LineComment
      | TokenKind::BlockComment
      | TokenKind::DocComment
      | TokenKind::DocBlockComment => self.include_comments,
      TokenKind::Whitespace => self.include_whitespace,
      _ => true,
    }
  }
}

impl<I> Iterator for TokenFilter<I>
where
  I: Iterator<Item = Result<Token, LexerError>>,
{
  type Item = Result<Token, LexerError>;

  fn next(&mut self) -> Option<Self::Item> {
    loop {
      match self.inner.next()? {
        Ok(token) => {
          if self.should_include(&token) {
            return Some(Ok(token));
          }
          // Continue to next token if this one should be filtered out
        }
        Err(error) => {
          return Some(Err(error));
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::errors::{ErrorRecoveryConfig, LexerError};
  use crate::lexer::tokens::{SourceLocation, Token, TokenKind};

  // Mock implementation for testing
  #[derive(Debug)]
  struct MockTokenStream {
    tokens: Vec<Result<Token, LexerError>>,
    position: usize,
  }

  impl MockTokenStream {
    fn new(tokens: Vec<Result<Token, LexerError>>) -> Self {
      Self {
        tokens,
        position: 0,
      }
    }
  }

  impl Iterator for MockTokenStream {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
      if self.position < self.tokens.len() {
        let token = self.tokens[self.position].clone();
        self.position += 1;
        Some(token)
      } else {
        None
      }
    }
  }

  impl TokenStream for MockTokenStream {
    fn peek(&self) -> Option<&Token> {
      if self.position < self.tokens.len() {
        match &self.tokens[self.position] {
          Ok(token) => Some(token),
          Err(_) => None,
        }
      } else {
        None
      }
    }

    fn position(&self) -> usize {
      self.position
    }

    fn is_eof(&self) -> bool {
      self.position >= self.tokens.len()
    }

    fn remaining(&self) -> Vec<Token> {
      self.tokens[self.position..]
        .iter()
        .filter_map(|r| r.as_ref().ok())
        .cloned()
        .collect()
    }

    fn reset(&mut self) {
      self.position = 0;
    }
  }

  // Mock implementation for ErrorHandler trait
  struct MockErrorHandler {
    errors: Vec<LexerError>,
    should_continue: bool,
  }

  impl MockErrorHandler {
    fn new(should_continue: bool) -> Self {
      Self {
        errors: Vec::new(),
        should_continue,
      }
    }
  }

  impl ErrorHandler for MockErrorHandler {
    fn handle_error(&mut self, error: &LexerError) -> bool {
      self.errors.push(error.clone());
      self.should_continue
    }

    fn get_errors(&self) -> &[LexerError] {
      &self.errors
    }

    fn clear_errors(&mut self) {
      self.errors.clear();
    }

    fn has_errors(&self) -> bool {
      !self.errors.is_empty()
    }

    fn error_count(&self) -> usize {
      self.errors.len()
    }
  }

  // Mock implementation for LexerConfig trait
  struct MockLexerConfig {
    error_recovery: ErrorRecoveryConfig,
    include_comments: bool,
    include_whitespace: bool,
  }

  impl MockLexerConfig {
    fn new(
      error_recovery: ErrorRecoveryConfig,
      include_comments: bool,
      include_whitespace: bool,
    ) -> Self {
      Self {
        error_recovery,
        include_comments,
        include_whitespace,
      }
    }
  }

  impl LexerConfig for MockLexerConfig {
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
      if self.include_comments && self.include_whitespace {
        Ok(())
      } else {
        Err("Configuration validation failed".to_string())
      }
    }
  }

  // Mock implementation for LexerBuilder trait
  struct MockLexerBuilder {
    error_recovery: Option<ErrorRecoveryConfig>,
    include_comments: Option<bool>,
    include_whitespace: Option<bool>,
  }

  impl MockLexerBuilder {
    fn new() -> Self {
      Self {
        error_recovery: None,
        include_comments: None,
        include_whitespace: None,
      }
    }
  }

  impl LexerBuilder for MockLexerBuilder {
    fn with_error_recovery(self, config: ErrorRecoveryConfig) -> Self {
      Self {
        error_recovery: Some(config),
        ..self
      }
    }

    fn with_comments(self, include: bool) -> Self {
      Self {
        include_comments: Some(include),
        ..self
      }
    }

    fn with_whitespace(self, include: bool) -> Self {
      Self {
        include_whitespace: Some(include),
        ..self
      }
    }

    fn build(self) -> Result<Box<dyn TokenStream>, String> {
      if self.error_recovery.is_some()
        && self.include_comments.is_some()
        && self.include_whitespace.is_some()
      {
        let tokens = vec![Ok(Token::new(
          TokenKind::Let,
          "let".to_string(),
          SourceLocation::start(),
          SourceLocation::start(),
        ))];
        Ok(Box::new(MockTokenStream::new(tokens)))
      } else {
        Err("Builder configuration incomplete".to_string())
      }
    }
  }

  // Helper function to create test tokens
  fn create_test_token(kind: TokenKind, text: &str) -> Token {
    Token::new(
      kind,
      text.to_string(),
      SourceLocation::start(),
      SourceLocation::start(),
    )
  }

  // Helper function to create test errors
  fn create_test_error() -> LexerError {
    LexerError::InvalidCharacter('@', SourceLocation::start())
  }

  // ===== TokenStream Tests =====

  #[test]
  fn test_token_stream_peek() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Let, "let")),
      Ok(create_test_token(TokenKind::Identifier, "x")),
    ];

    let stream = MockTokenStream::new(tokens);
    let peeked = stream.peek();
    assert!(peeked.is_some());
    assert_eq!(peeked.unwrap().kind, TokenKind::Let);
  }

  #[test]
  fn test_token_stream_peek_at_eof() {
    let tokens = vec![];
    let stream = MockTokenStream::new(tokens);
    let peeked = stream.peek();
    assert!(peeked.is_none());
  }

  #[test]
  fn test_token_stream_peek_with_error() {
    let tokens = vec![
      Err(create_test_error()),
      Ok(create_test_token(TokenKind::Let, "let")),
    ];

    let stream = MockTokenStream::new(tokens);
    let peeked = stream.peek();
    assert!(peeked.is_none()); // Errors are not peekable
  }

  #[test]
  fn test_token_stream_position() {
    let tokens = vec![Ok(create_test_token(TokenKind::Let, "let"))];
    let stream = MockTokenStream::new(tokens);
    assert_eq!(stream.position(), 0);
  }

  #[test]
  fn test_token_stream_position_after_consumption() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Let, "let")),
      Ok(create_test_token(TokenKind::Identifier, "x")),
    ];

    let mut stream = MockTokenStream::new(tokens);
    let _ = stream.next();
    assert_eq!(stream.position(), 1);
  }

  #[test]
  fn test_token_stream_is_eof() {
    let tokens = vec![];
    let stream = MockTokenStream::new(tokens);
    assert!(stream.is_eof());

    let tokens = vec![Ok(create_test_token(TokenKind::Let, "let"))];
    let stream = MockTokenStream::new(tokens);
    assert!(!stream.is_eof());
  }

  #[test]
  fn test_token_stream_is_eof_after_consumption() {
    let tokens = vec![Ok(create_test_token(TokenKind::Let, "let"))];
    let mut stream = MockTokenStream::new(tokens);
    let _ = stream.next();
    assert!(stream.is_eof());
  }

  #[test]
  fn test_token_stream_remaining() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Let, "let")),
      Ok(create_test_token(TokenKind::Identifier, "x")),
      Ok(create_test_token(TokenKind::Semicolon, ";")),
    ];

    let stream = MockTokenStream::new(tokens);
    let remaining = stream.remaining();
    assert_eq!(remaining.len(), 3);
    assert_eq!(remaining[0].kind, TokenKind::Let);
    assert_eq!(remaining[1].kind, TokenKind::Identifier);
    assert_eq!(remaining[2].kind, TokenKind::Semicolon);
  }

  #[test]
  fn test_token_stream_remaining_with_errors() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Let, "let")),
      Err(create_test_error()),
      Ok(create_test_token(TokenKind::Identifier, "x")),
    ];

    let stream = MockTokenStream::new(tokens);
    let remaining = stream.remaining();
    assert_eq!(remaining.len(), 2); // Errors are filtered out
    assert_eq!(remaining[0].kind, TokenKind::Let);
    assert_eq!(remaining[1].kind, TokenKind::Identifier);
  }

  #[test]
  fn test_token_stream_remaining_empty() {
    let tokens = vec![];
    let stream = MockTokenStream::new(tokens);
    let remaining = stream.remaining();
    assert!(remaining.is_empty());
  }

  #[test]
  fn test_token_stream_reset() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Let, "let")),
      Ok(create_test_token(TokenKind::Identifier, "x")),
    ];

    let mut stream = MockTokenStream::new(tokens);

    // Consume first token
    let _ = stream.next();
    assert_eq!(stream.position(), 1);

    // Reset
    stream.reset();
    assert_eq!(stream.position(), 0);

    // Should be able to consume tokens again
    let first = stream.next();
    assert!(first.is_some());
    assert_eq!(first.unwrap().unwrap().kind, TokenKind::Let);
  }

  // ===== ErrorHandler Tests =====

  #[test]
  fn test_error_handler_handle_error_continue() {
    let mut handler = MockErrorHandler::new(true);
    let error = create_test_error();

    let result = handler.handle_error(&error);
    assert!(result);
    assert_eq!(handler.error_count(), 1);
  }

  #[test]
  fn test_error_handler_handle_error_stop() {
    let mut handler = MockErrorHandler::new(false);
    let error = create_test_error();

    let result = handler.handle_error(&error);
    assert!(!result);
    assert_eq!(handler.error_count(), 1);
  }

  #[test]
  fn test_error_handler_get_errors() {
    let mut handler = MockErrorHandler::new(true);
    let error1 = create_test_error();
    let error2 = LexerError::UnterminatedString(SourceLocation::start());

    handler.handle_error(&error1);
    handler.handle_error(&error2);

    let errors = handler.get_errors();
    assert_eq!(errors.len(), 2);
    assert_eq!(errors[0], error1);
    assert_eq!(errors[1], error2);
  }

  #[test]
  fn test_error_handler_clear_errors() {
    let mut handler = MockErrorHandler::new(true);
    let error = create_test_error();

    handler.handle_error(&error);
    assert_eq!(handler.error_count(), 1);

    handler.clear_errors();
    assert_eq!(handler.error_count(), 0);
    assert!(!handler.has_errors());
  }

  #[test]
  fn test_error_handler_has_errors() {
    let mut handler = MockErrorHandler::new(true);
    assert!(!handler.has_errors());

    let error = create_test_error();
    handler.handle_error(&error);
    assert!(handler.has_errors());
  }

  #[test]
  fn test_error_handler_error_count() {
    let mut handler = MockErrorHandler::new(true);
    assert_eq!(handler.error_count(), 0);

    let error1 = create_test_error();
    let error2 = LexerError::UnterminatedString(SourceLocation::start());

    handler.handle_error(&error1);
    assert_eq!(handler.error_count(), 1);

    handler.handle_error(&error2);
    assert_eq!(handler.error_count(), 2);
  }

  // ===== LexerConfig Tests =====

  #[test]
  fn test_lexer_config_error_recovery() {
    let config = ErrorRecoveryConfig::default();
    let lexer_config = MockLexerConfig::new(config.clone(), true, true);

    let retrieved_config = lexer_config.error_recovery();
    assert_eq!(retrieved_config, &config);
  }

  #[test]
  fn test_lexer_config_include_comments() {
    let lexer_config = MockLexerConfig::new(ErrorRecoveryConfig::default(), true, false);
    assert!(lexer_config.include_comments());

    let lexer_config = MockLexerConfig::new(ErrorRecoveryConfig::default(), false, false);
    assert!(!lexer_config.include_comments());
  }

  #[test]
  fn test_lexer_config_include_whitespace() {
    let lexer_config = MockLexerConfig::new(ErrorRecoveryConfig::default(), false, true);
    assert!(lexer_config.include_whitespace());

    let lexer_config = MockLexerConfig::new(ErrorRecoveryConfig::default(), false, false);
    assert!(!lexer_config.include_whitespace());
  }

  #[test]
  fn test_lexer_config_validate_success() {
    let lexer_config = MockLexerConfig::new(ErrorRecoveryConfig::default(), true, true);
    let result = lexer_config.validate();
    assert!(result.is_ok());
  }

  #[test]
  fn test_lexer_config_validate_failure() {
    let lexer_config = MockLexerConfig::new(ErrorRecoveryConfig::default(), false, false);
    let result = lexer_config.validate();
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "Configuration validation failed");
  }

  // ===== LexerBuilder Tests =====

  #[test]
  fn test_lexer_builder_with_error_recovery() {
    let config = ErrorRecoveryConfig::strict();
    let builder = MockLexerBuilder::new().with_error_recovery(config.clone());

    // We can't directly test the private fields, but we can test the build method
    // which will fail if the configuration wasn't set properly
    let result = builder.build();
    match result {
      Ok(_) => panic!("Expected build to fail"),
      Err(_) => (), // Expected to fail
    }
  }

  #[test]
  fn test_lexer_builder_with_comments() {
    let builder = MockLexerBuilder::new().with_comments(true);
    let result = builder.build();
    match result {
      Ok(_) => panic!("Expected build to fail"),
      Err(_) => (), // Expected to fail - still missing other required configs
    }
  }

  #[test]
  fn test_lexer_builder_with_whitespace() {
    let builder = MockLexerBuilder::new().with_whitespace(false);
    let result = builder.build();
    match result {
      Ok(_) => panic!("Expected build to fail"),
      Err(_) => (), // Expected to fail - still missing other required configs
    }
  }

  #[test]
  fn test_lexer_builder_build_success() {
    let config = ErrorRecoveryConfig::default();
    let builder = MockLexerBuilder::new()
      .with_error_recovery(config)
      .with_comments(true)
      .with_whitespace(true);

    let result = builder.build();
    match result {
      Ok(token_stream) => {
        let remaining = token_stream.remaining();
        assert_eq!(remaining.len(), 1);
        assert_eq!(remaining[0].kind, TokenKind::Let);
      }
      Err(msg) => panic!("Expected build to succeed, but got error: {}", msg),
    }
  }

  #[test]
  fn test_lexer_builder_build_failure() {
    let builder = MockLexerBuilder::new();
    let result = builder.build();
    match result {
      Ok(_) => panic!("Expected build to fail"),
      Err(msg) => assert_eq!(msg, "Builder configuration incomplete"),
    }
  }

  // ===== TokenFilter Tests =====

  #[test]
  fn test_token_filter_includes_all_tokens_when_enabled() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Let, "let")),
      Ok(create_test_token(TokenKind::Whitespace, " ")),
      Ok(create_test_token(TokenKind::Identifier, "x")),
      Ok(create_test_token(TokenKind::LineComment, "// comment")),
    ];

    let stream = MockTokenStream::new(tokens);
    let filter = TokenFilter::new(stream, true, true);

    let filtered: Vec<_> = filter.collect();
    assert_eq!(filtered.len(), 4);
  }

  #[test]
  fn test_token_filter_excludes_comments_when_disabled() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Let, "let")),
      Ok(create_test_token(TokenKind::LineComment, "// comment")),
      Ok(create_test_token(TokenKind::Identifier, "x")),
    ];

    let stream = MockTokenStream::new(tokens);
    let filter = TokenFilter::new(stream, false, true);

    let filtered: Vec<_> = filter.collect();
    assert_eq!(filtered.len(), 2);
    assert_eq!(filtered[0].as_ref().unwrap().kind, TokenKind::Let);
    assert_eq!(filtered[1].as_ref().unwrap().kind, TokenKind::Identifier);
  }

  #[test]
  fn test_token_filter_excludes_whitespace_when_disabled() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Let, "let")),
      Ok(create_test_token(TokenKind::Whitespace, " ")),
      Ok(create_test_token(TokenKind::Identifier, "x")),
    ];

    let stream = MockTokenStream::new(tokens);
    let filter = TokenFilter::new(stream, true, false);

    let filtered: Vec<_> = filter.collect();
    assert_eq!(filtered.len(), 2);
    assert_eq!(filtered[0].as_ref().unwrap().kind, TokenKind::Let);
    assert_eq!(filtered[1].as_ref().unwrap().kind, TokenKind::Identifier);
  }

  #[test]
  fn test_token_filter_excludes_both_comments_and_whitespace_when_disabled() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Let, "let")),
      Ok(create_test_token(TokenKind::Whitespace, " ")),
      Ok(create_test_token(TokenKind::LineComment, "// comment")),
      Ok(create_test_token(TokenKind::Identifier, "x")),
      Ok(create_test_token(TokenKind::BlockComment, "/* block */")),
    ];

    let stream = MockTokenStream::new(tokens);
    let filter = TokenFilter::new(stream, false, false);

    let filtered: Vec<_> = filter.collect();
    assert_eq!(filtered.len(), 2);
    assert_eq!(filtered[0].as_ref().unwrap().kind, TokenKind::Let);
    assert_eq!(filtered[1].as_ref().unwrap().kind, TokenKind::Identifier);
  }

  #[test]
  fn test_token_filter_includes_doc_comments_when_comments_enabled() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Let, "let")),
      Ok(create_test_token(TokenKind::DocComment, "/// doc")),
      Ok(create_test_token(TokenKind::DocBlockComment, "/** doc */")),
      Ok(create_test_token(TokenKind::Identifier, "x")),
    ];

    let stream = MockTokenStream::new(tokens);
    let filter = TokenFilter::new(stream, true, true);

    let filtered: Vec<_> = filter.collect();
    assert_eq!(filtered.len(), 4);
  }

  #[test]
  fn test_token_filter_excludes_doc_comments_when_comments_disabled() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Let, "let")),
      Ok(create_test_token(TokenKind::LineComment, "// comment")),
      Ok(create_test_token(TokenKind::DocComment, "/// doc")),
      Ok(create_test_token(TokenKind::DocBlockComment, "/** doc */")),
      Ok(create_test_token(TokenKind::Identifier, "x")),
    ];

    let stream = MockTokenStream::new(tokens);
    let filter = TokenFilter::new(stream, false, true);

    let filtered: Vec<_> = filter.collect();
    assert_eq!(filtered.len(), 2);
    assert_eq!(filtered[0].as_ref().unwrap().kind, TokenKind::Let);
    assert_eq!(filtered[1].as_ref().unwrap().kind, TokenKind::Identifier);
  }

  #[test]
  fn test_token_filter_preserves_errors() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Let, "let")),
      Err(create_test_error()),
      Ok(create_test_token(TokenKind::Identifier, "x")),
    ];

    let stream = MockTokenStream::new(tokens);
    let filter = TokenFilter::new(stream, false, false);

    let filtered: Vec<_> = filter.collect();
    assert_eq!(filtered.len(), 3);

    // First token should be preserved
    assert!(filtered[0].is_ok());
    assert_eq!(filtered[0].as_ref().unwrap().kind, TokenKind::Let);

    // Error should be preserved
    assert!(filtered[1].is_err());

    // Last token should be preserved
    assert!(filtered[2].is_ok());
    assert_eq!(filtered[2].as_ref().unwrap().kind, TokenKind::Identifier);
  }

  #[test]
  fn test_token_filter_preserves_non_comment_whitespace_tokens() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Let, "let")),
      Ok(create_test_token(TokenKind::Identifier, "x")),
      Ok(create_test_token(TokenKind::Plus, "+")),
      Ok(create_test_token(TokenKind::Integer, "42")),
    ];

    let stream = MockTokenStream::new(tokens);
    let filter = TokenFilter::new(stream, false, false);

    let filtered: Vec<_> = filter.collect();
    assert_eq!(filtered.len(), 4);
    assert_eq!(filtered[0].as_ref().unwrap().kind, TokenKind::Let);
    assert_eq!(filtered[1].as_ref().unwrap().kind, TokenKind::Identifier);
    assert_eq!(filtered[2].as_ref().unwrap().kind, TokenKind::Plus);
    assert_eq!(filtered[3].as_ref().unwrap().kind, TokenKind::Integer);
  }

  #[test]
  fn test_token_filter_handles_empty_stream() {
    let tokens = vec![];
    let stream = MockTokenStream::new(tokens);
    let filter = TokenFilter::new(stream, true, true);

    let filtered: Vec<_> = filter.collect();
    assert!(filtered.is_empty());
  }

  #[test]
  fn test_token_filter_handles_only_filtered_tokens() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Whitespace, " ")),
      Ok(create_test_token(TokenKind::LineComment, "// comment")),
      Ok(create_test_token(TokenKind::BlockComment, "/* block */")),
    ];

    let stream = MockTokenStream::new(tokens);
    let filter = TokenFilter::new(stream, false, false);

    let filtered: Vec<_> = filter.collect();
    assert!(filtered.is_empty());
  }

  #[test]
  fn test_token_filter_handles_mixed_content() {
    let tokens = vec![
      Ok(create_test_token(TokenKind::Whitespace, " ")),
      Ok(create_test_token(TokenKind::Let, "let")),
      Ok(create_test_token(TokenKind::LineComment, "// comment")),
      Ok(create_test_token(TokenKind::Whitespace, " ")),
      Ok(create_test_token(TokenKind::Identifier, "x")),
      Ok(create_test_token(TokenKind::BlockComment, "/* block */")),
      Ok(create_test_token(TokenKind::Semicolon, ";")),
    ];

    let stream = MockTokenStream::new(tokens);
    let filter = TokenFilter::new(stream, false, false);

    let filtered: Vec<_> = filter.collect();
    assert_eq!(filtered.len(), 3);
    assert_eq!(filtered[0].as_ref().unwrap().kind, TokenKind::Let);
    assert_eq!(filtered[1].as_ref().unwrap().kind, TokenKind::Identifier);
    assert_eq!(filtered[2].as_ref().unwrap().kind, TokenKind::Semicolon);
  }
}
