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
pub trait TokenStream: Iterator<Item = Result<Token, LexerError>> {
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
  use crate::lexer::errors::LexerError;
  use crate::lexer::tokens::{SourceLocation, Token, TokenKind};

  // Mock implementation for testing
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

  #[test]
  fn test_token_filter_includes_all_tokens_when_enabled() {
    let tokens = vec![
      Ok(Token::new(
        TokenKind::Let,
        "let".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
      Ok(Token::new(
        TokenKind::Whitespace,
        " ".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
      Ok(Token::new(
        TokenKind::Identifier,
        "x".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
      Ok(Token::new(
        TokenKind::LineComment,
        "// comment".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
    ];

    let stream = MockTokenStream::new(tokens);
    let mut filter = TokenFilter::new(stream, true, true);

    let filtered: Vec<_> = filter.collect();
    assert_eq!(filtered.len(), 4);
  }

  #[test]
  fn test_token_filter_excludes_comments_when_disabled() {
    let tokens = vec![
      Ok(Token::new(
        TokenKind::Let,
        "let".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
      Ok(Token::new(
        TokenKind::LineComment,
        "// comment".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
      Ok(Token::new(
        TokenKind::Identifier,
        "x".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
    ];

    let stream = MockTokenStream::new(tokens);
    let mut filter = TokenFilter::new(stream, false, true);

    let filtered: Vec<_> = filter.collect();
    assert_eq!(filtered.len(), 2);
  }

  #[test]
  fn test_token_filter_excludes_whitespace_when_disabled() {
    let tokens = vec![
      Ok(Token::new(
        TokenKind::Let,
        "let".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
      Ok(Token::new(
        TokenKind::Whitespace,
        " ".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
      Ok(Token::new(
        TokenKind::Identifier,
        "x".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
    ];

    let stream = MockTokenStream::new(tokens);
    let mut filter = TokenFilter::new(stream, true, false);

    let filtered: Vec<_> = filter.collect();
    assert_eq!(filtered.len(), 2);
  }

  #[test]
  fn test_token_stream_peek() {
    let tokens = vec![
      Ok(Token::new(
        TokenKind::Let,
        "let".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
      Ok(Token::new(
        TokenKind::Identifier,
        "x".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
    ];

    let stream = MockTokenStream::new(tokens);
    let peeked = stream.peek();
    assert!(peeked.is_some());
    assert_eq!(peeked.unwrap().kind, TokenKind::Let);
  }

  #[test]
  fn test_token_stream_position() {
    let tokens = vec![Ok(Token::new(
      TokenKind::Let,
      "let".to_string(),
      SourceLocation::start(),
      SourceLocation::start(),
    ))];

    let stream = MockTokenStream::new(tokens);
    assert_eq!(stream.position(), 0);
  }

  #[test]
  fn test_token_stream_is_eof() {
    let tokens = vec![];
    let stream = MockTokenStream::new(tokens);
    assert!(stream.is_eof());

    let tokens = vec![Ok(Token::new(
      TokenKind::Let,
      "let".to_string(),
      SourceLocation::start(),
      SourceLocation::start(),
    ))];
    let stream = MockTokenStream::new(tokens);
    assert!(!stream.is_eof());
  }

  #[test]
  fn test_token_stream_reset() {
    let tokens = vec![
      Ok(Token::new(
        TokenKind::Let,
        "let".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
      Ok(Token::new(
        TokenKind::Identifier,
        "x".to_string(),
        SourceLocation::start(),
        SourceLocation::start(),
      )),
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
}
