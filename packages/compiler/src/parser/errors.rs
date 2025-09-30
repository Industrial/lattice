//! Error handling and reporting for the Lattice language parser.
//!
//! This module provides comprehensive error types, error recovery mechanisms,
//! and integration with the codespan-reporting crate for detailed error messages.

use crate::lexer::{SourceLocation, Token, TokenKind};
use crate::parser::ast::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::{self, termcolor};
use std::fmt;
use thiserror::Error;

/// Represents a parse error with detailed information
#[derive(Debug, Clone, Error)]
pub struct ParseError {
  /// The kind of parse error
  pub kind: ParseErrorKind,
  /// The location where the error occurred
  pub location: SourceLocation,
  /// Additional context about the error
  pub context: Option<String>,
  /// Expected tokens or constructs
  pub expected: Vec<String>,
  /// The actual token or construct found
  pub found: Option<String>,
  /// Suggestions for fixing the error
  pub suggestions: Vec<String>,
}

impl ParseError {
  /// Create a new parse error
  pub fn new(kind: ParseErrorKind, location: SourceLocation) -> Self {
    Self {
      kind,
      location,
      context: None,
      expected: Vec::new(),
      found: None,
      suggestions: Vec::new(),
    }
  }

  /// Add context to the error
  pub fn with_context(mut self, context: String) -> Self {
    self.context = Some(context);
    self
  }

  /// Add expected tokens/constructs to the error
  pub fn with_expected(mut self, expected: Vec<String>) -> Self {
    self.expected = expected;
    self
  }

  /// Add the found token/construct to the error
  pub fn with_found(mut self, found: String) -> Self {
    self.found = Some(found);
    self
  }

  /// Add suggestions for fixing the error
  pub fn with_suggestions(mut self, suggestions: Vec<String>) -> Self {
    self.suggestions = suggestions;
    self
  }

  /// Get the span of the error
  pub fn span(&self) -> Span {
    Span::new(self.location, self.location)
  }

  /// Convert the error to a codespan diagnostic
  pub fn to_diagnostic(&self, file_id: usize) -> Diagnostic<usize> {
    let mut diagnostic = Diagnostic::error()
      .with_message(self.kind.to_string())
      .with_labels(vec![Label::primary(
        file_id,
        self.location.offset..self.location.offset + 1,
      )
      .with_message(self.kind.description())]);

    if let Some(context) = &self.context {
      diagnostic = diagnostic.with_notes(vec![context.clone()]);
    }

    if !self.expected.is_empty() {
      let expected_msg = format!("Expected: {}", self.expected.join(", "));
      diagnostic = diagnostic.with_notes(vec![expected_msg]);
    }

    if let Some(found) = &self.found {
      let found_msg = format!("Found: {}", found);
      diagnostic = diagnostic.with_notes(vec![found_msg]);
    }

    if !self.suggestions.is_empty() {
      let suggestions_msg = format!("Suggestions: {}", self.suggestions.join("; "));
      diagnostic = diagnostic.with_notes(vec![suggestions_msg]);
    }

    diagnostic
  }
}

impl From<crate::lexer::LexerError> for ParseError {
  fn from(error: crate::lexer::LexerError) -> Self {
    ParseError::new(
      ParseErrorKind::LexerError,
      crate::lexer::SourceLocation::start(),
    )
    .with_context(format!("Lexer error: {}", error))
  }
}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} at {}", self.kind, self.location)?;

    if let Some(context) = &self.context {
      write!(f, "\nContext: {}", context)?;
    }

    if !self.expected.is_empty() {
      write!(f, "\nExpected: {}", self.expected.join(", "))?;
    }

    if let Some(found) = &self.found {
      write!(f, "\nFound: {}", found)?;
    }

    if !self.suggestions.is_empty() {
      write!(f, "\nSuggestions: {}", self.suggestions.join("; "))?;
    }

    Ok(())
  }
}

/// Represents different kinds of parse errors
#[derive(Debug, Clone, Error, PartialEq)]
pub enum ParseErrorKind {
  /// Unexpected end of input
  #[error("Unexpected end of input")]
  UnexpectedEndOfInput,

  /// Unexpected token
  #[error("Unexpected token")]
  UnexpectedToken,

  /// Expected a specific token
  #[error("Expected token")]
  ExpectedToken,

  /// Expected a specific construct
  #[error("Expected construct")]
  ExpectedConstruct,

  /// Invalid expression
  #[error("Invalid expression")]
  InvalidExpression,

  /// Invalid pattern
  #[error("Invalid pattern")]
  InvalidPattern,

  /// Invalid type expression
  #[error("Invalid type expression")]
  InvalidTypeExpression,

  /// Invalid statement
  #[error("Invalid statement")]
  InvalidStatement,

  /// Invalid declaration
  #[error("Invalid declaration")]
  InvalidDeclaration,

  /// Duplicate identifier
  #[error("Duplicate identifier")]
  DuplicateIdentifier,

  /// Invalid operator precedence
  #[error("Invalid operator precedence")]
  InvalidOperatorPrecedence,

  /// Missing semicolon
  #[error("Missing semicolon")]
  MissingSemicolon,

  /// Missing closing delimiter
  #[error("Missing closing delimiter")]
  MissingClosingDelimiter,

  /// Invalid literal
  #[error("Invalid literal")]
  InvalidLiteral,

  /// Recursive type definition
  #[error("Recursive type definition")]
  RecursiveTypeDefinition,

  /// Invalid effect handler
  #[error("Invalid effect handler")]
  InvalidEffectHandler,

  /// Invalid pattern matching
  #[error("Invalid pattern matching")]
  InvalidPatternMatching,

  /// Lexer error
  #[error("Lexer error")]
  LexerError,
}

impl ParseErrorKind {
  /// Get a human-readable description of the error
  pub fn description(&self) -> &'static str {
    match self {
      ParseErrorKind::UnexpectedEndOfInput => "Unexpected end of input",
      ParseErrorKind::UnexpectedToken => "Unexpected token encountered",
      ParseErrorKind::ExpectedToken => "Expected a specific token",
      ParseErrorKind::ExpectedConstruct => "Expected a specific language construct",
      ParseErrorKind::InvalidExpression => "Invalid expression syntax",
      ParseErrorKind::InvalidPattern => "Invalid pattern syntax",
      ParseErrorKind::InvalidTypeExpression => "Invalid type expression syntax",
      ParseErrorKind::InvalidStatement => "Invalid statement syntax",
      ParseErrorKind::InvalidDeclaration => "Invalid declaration syntax",
      ParseErrorKind::DuplicateIdentifier => "Identifier already defined",
      ParseErrorKind::InvalidOperatorPrecedence => "Invalid operator precedence",
      ParseErrorKind::MissingSemicolon => "Missing semicolon",
      ParseErrorKind::MissingClosingDelimiter => "Missing closing delimiter",
      ParseErrorKind::InvalidLiteral => "Invalid literal value",
      ParseErrorKind::RecursiveTypeDefinition => "Recursive type definition not allowed",
      ParseErrorKind::InvalidEffectHandler => "Invalid effect handler syntax",
      ParseErrorKind::InvalidPatternMatching => "Invalid pattern matching syntax",
      ParseErrorKind::LexerError => "Lexer error occurred",
    }
  }
}

/// Result type for parser operations
pub type ParseResult<T> = Result<T, ParseError>;

/// Configuration for error recovery during parsing
#[derive(Debug, Clone)]
pub struct ErrorRecoveryConfig {
  /// Whether to continue parsing after encountering errors
  pub continue_on_error: bool,
  /// Maximum number of errors to collect before stopping
  pub max_errors: Option<usize>,
  /// Whether to insert error tokens for recovery
  pub insert_error_tokens: bool,
  /// Whether to skip invalid characters
  pub skip_invalid_chars: bool,
  /// Whether to attempt to recover from common mistakes
  pub attempt_recovery: bool,
}

impl Default for ErrorRecoveryConfig {
  fn default() -> Self {
    Self {
      continue_on_error: false,
      max_errors: Some(10),
      insert_error_tokens: false,
      skip_invalid_chars: false,
      attempt_recovery: true,
    }
  }
}

/// Error recovery strategies for different error types
#[derive(Debug, Clone)]
pub enum RecoveryStrategy {
  /// Skip tokens until a specific token is found
  SkipUntil(Vec<TokenKind>),
  /// Skip tokens until a specific construct is found
  SkipUntilConstruct(String),
  /// Insert a missing token
  InsertToken(TokenKind),
  /// Delete the current token
  DeleteToken,
  /// Replace the current token with another
  ReplaceToken(TokenKind),
  /// Try to parse with a different precedence
  TryDifferentPrecedence,
  /// Skip to the next statement boundary
  SkipToStatementBoundary,
  /// Skip to the next declaration boundary
  SkipToDeclarationBoundary,
  /// Give up on this construct and continue
  GiveUpAndContinue,
}

impl RecoveryStrategy {
  /// Get a description of the recovery strategy
  pub fn description(&self) -> &'static str {
    match self {
      RecoveryStrategy::SkipUntil(_) => "Skip until specific token",
      RecoveryStrategy::SkipUntilConstruct(_) => "Skip until specific construct",
      RecoveryStrategy::InsertToken(_) => "Insert missing token",
      RecoveryStrategy::DeleteToken => "Delete current token",
      RecoveryStrategy::ReplaceToken(_) => "Replace current token",
      RecoveryStrategy::TryDifferentPrecedence => "Try different precedence",
      RecoveryStrategy::SkipToStatementBoundary => "Skip to statement boundary",
      RecoveryStrategy::SkipToDeclarationBoundary => "Skip to declaration boundary",
      RecoveryStrategy::GiveUpAndContinue => "Give up and continue",
    }
  }
}

/// Error recovery context for maintaining state during recovery
#[derive(Debug, Clone)]
pub struct ErrorRecoveryContext {
  /// The current recovery strategy
  pub strategy: RecoveryStrategy,
  /// Number of tokens skipped during recovery
  pub tokens_skipped: usize,
  /// Whether recovery was successful
  pub successful: bool,
  /// Additional context for the recovery
  pub context: Option<String>,
}

impl ErrorRecoveryContext {
  /// Create a new error recovery context
  pub fn new(strategy: RecoveryStrategy) -> Self {
    Self {
      strategy,
      tokens_skipped: 0,
      successful: false,
      context: None,
    }
  }

  /// Mark recovery as successful
  pub fn mark_successful(&mut self) {
    self.successful = true;
  }

  /// Add context to the recovery
  pub fn with_context(mut self, context: String) -> Self {
    self.context = Some(context);
    self
  }
}

/// Error reporter for formatting and displaying parse errors
pub struct ErrorReporter {
  /// The source code being parsed
  source: String,
  /// The file name (if any)
  file_name: Option<String>,
}

impl ErrorReporter {
  /// Create a new error reporter
  pub fn new(source: String) -> Self {
    Self {
      source,
      file_name: None,
    }
  }

  /// Set the file name for the error reporter
  pub fn with_file_name(mut self, file_name: String) -> Self {
    self.file_name = Some(file_name);
    self
  }

  /// Report a single parse error
  pub fn report_error(&self, error: &ParseError) -> Result<(), Box<dyn std::error::Error>> {
    let file_name = self.file_name.as_deref().unwrap_or("input");
    let file = SimpleFile::new(file_name, self.source.clone());

    let diagnostic = Diagnostic::error()
      .with_message(error.kind.to_string())
      .with_labels(vec![Label::primary(
        (),
        error.location.offset..error.location.offset + 1,
      )
      .with_message(error.kind.description())]);

    let mut writer = termcolor::Buffer::ansi();
    term::emit(&mut writer, &term::Config::default(), &file, &diagnostic)?;

    let buffer = writer.into_inner();
    let output = String::from_utf8_lossy(&buffer);
    eprintln!("{}", output);

    Ok(())
  }

  /// Report multiple parse errors
  pub fn report_errors(&self, errors: &[ParseError]) -> Result<(), Box<dyn std::error::Error>> {
    for error in errors {
      self.report_error(error)?;
    }
    Ok(())
  }

  /// Generate a summary of all errors
  pub fn generate_summary(&self, errors: &[ParseError]) -> String {
    if errors.is_empty() {
      return "No errors found.".to_string();
    }

    let mut summary = format!("Found {} parse error(s):\n", errors.len());

    for (i, error) in errors.iter().enumerate() {
      summary.push_str(&format!(
        "{}. {} at {}\n",
        i + 1,
        error.kind,
        error.location
      ));

      if let Some(context) = &error.context {
        summary.push_str(&format!("   Context: {}\n", context));
      }

      if !error.expected.is_empty() {
        summary.push_str(&format!("   Expected: {}\n", error.expected.join(", ")));
      }

      if let Some(found) = &error.found {
        summary.push_str(&format!("   Found: {}\n", found));
      }

      if !error.suggestions.is_empty() {
        summary.push_str(&format!(
          "   Suggestions: {}\n",
          error.suggestions.join("; ")
        ));
      }

      summary.push('\n');
    }

    summary
  }
}

/// Helper functions for creating common parse errors
pub mod error_helpers {
  use super::*;

  /// Create an "unexpected token" error
  pub fn unexpected_token(token: &Token, expected: Vec<String>) -> ParseError {
    ParseError::new(ParseErrorKind::UnexpectedToken, token.start)
      .with_found(token.text.clone())
      .with_expected(expected)
      .with_suggestions(vec![
        "Check the syntax around this token".to_string(),
        "Ensure proper spacing and delimiters".to_string(),
      ])
  }

  /// Create an "expected token" error
  pub fn expected_token(
    location: SourceLocation,
    expected: Vec<String>,
    found: Option<String>,
  ) -> ParseError {
    let mut error =
      ParseError::new(ParseErrorKind::ExpectedToken, location).with_expected(expected);

    if let Some(found) = found {
      error = error.with_found(found);
    }

    error.with_suggestions(vec![
      "Insert the expected token".to_string(),
      "Check for missing delimiters or keywords".to_string(),
    ])
  }

  /// Create an "unexpected end of input" error
  pub fn unexpected_end_of_input(location: SourceLocation, expected: Vec<String>) -> ParseError {
    ParseError::new(ParseErrorKind::UnexpectedEndOfInput, location)
      .with_expected(expected)
      .with_suggestions(vec![
        "Check for missing closing delimiters".to_string(),
        "Ensure the input is complete".to_string(),
      ])
  }

  /// Create an "invalid expression" error
  pub fn invalid_expression(location: SourceLocation, context: String) -> ParseError {
    ParseError::new(ParseErrorKind::InvalidExpression, location)
      .with_context(context)
      .with_suggestions(vec![
        "Check the expression syntax".to_string(),
        "Ensure proper operator precedence".to_string(),
      ])
  }

  /// Create a "missing semicolon" error
  pub fn missing_semicolon(location: SourceLocation) -> ParseError {
    ParseError::new(ParseErrorKind::MissingSemicolon, location).with_suggestions(vec![
      "Add a semicolon after this statement".to_string(),
      "Check for proper statement termination".to_string(),
    ])
  }

  /// Create a "missing closing delimiter" error
  pub fn missing_closing_delimiter(location: SourceLocation, expected: String) -> ParseError {
    ParseError::new(ParseErrorKind::MissingClosingDelimiter, location)
      .with_expected(vec![expected])
      .with_suggestions(vec![
        "Add the missing closing delimiter".to_string(),
        "Check for balanced parentheses, brackets, or braces".to_string(),
      ])
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::TokenKind;

  #[test]
  fn test_parse_error_creation() {
    let location = SourceLocation::new(1, 1, 0);
    let error = ParseError::new(ParseErrorKind::UnexpectedToken, location);

    assert_eq!(error.kind, ParseErrorKind::UnexpectedToken);
    assert_eq!(error.location, location);
    assert!(error.context.is_none());
    assert!(error.expected.is_empty());
    assert!(error.found.is_none());
    assert!(error.suggestions.is_empty());
  }

  #[test]
  fn test_parse_error_with_context() {
    let location = SourceLocation::new(1, 1, 0);
    let error = ParseError::new(ParseErrorKind::UnexpectedToken, location)
      .with_context("While parsing function definition".to_string());

    assert_eq!(
      error.context,
      Some("While parsing function definition".to_string())
    );
  }

  #[test]
  fn test_parse_error_with_expected() {
    let location = SourceLocation::new(1, 1, 0);
    let error = ParseError::new(ParseErrorKind::ExpectedToken, location)
      .with_expected(vec![";".to_string(), "}".to_string()]);

    assert_eq!(error.expected, vec![";".to_string(), "}".to_string()]);
  }

  #[test]
  fn test_parse_error_with_found() {
    let location = SourceLocation::new(1, 1, 0);
    let error =
      ParseError::new(ParseErrorKind::UnexpectedToken, location).with_found("+".to_string());

    assert_eq!(error.found, Some("+".to_string()));
  }

  #[test]
  fn test_parse_error_with_suggestions() {
    let location = SourceLocation::new(1, 1, 0);
    let error = ParseError::new(ParseErrorKind::UnexpectedToken, location)
      .with_suggestions(vec!["Check syntax".to_string(), "Fix spacing".to_string()]);

    assert_eq!(
      error.suggestions,
      vec!["Check syntax".to_string(), "Fix spacing".to_string()]
    );
  }

  #[test]
  fn test_error_recovery_config_default() {
    let config = ErrorRecoveryConfig::default();

    assert!(!config.continue_on_error);
    assert_eq!(config.max_errors, Some(10));
    assert!(!config.insert_error_tokens);
    assert!(!config.skip_invalid_chars);
    assert!(config.attempt_recovery);
  }

  #[test]
  fn test_recovery_strategy_description() {
    let strategy = RecoveryStrategy::SkipUntil(vec![TokenKind::Semicolon]);
    assert_eq!(strategy.description(), "Skip until specific token");
  }

  #[test]
  fn test_error_recovery_context() {
    let strategy = RecoveryStrategy::SkipUntil(vec![TokenKind::Semicolon]);
    let mut context = ErrorRecoveryContext::new(strategy);

    assert!(!context.successful);
    assert_eq!(context.tokens_skipped, 0);

    context.mark_successful();
    assert!(context.successful);
  }

  #[test]
  fn test_error_helpers() {
    let location = SourceLocation::new(1, 1, 0);
    let token = crate::lexer::Token::new(TokenKind::Plus, "+".to_string(), location, location);

    let error = error_helpers::unexpected_token(&token, vec![";".to_string()]);
    assert_eq!(error.kind, ParseErrorKind::UnexpectedToken);
    assert_eq!(error.found, Some("+".to_string()));
    assert_eq!(error.expected, vec![";".to_string()]);
  }

  #[test]
  fn test_error_reporter_creation() {
    let source = "let x = 42".to_string();
    let reporter = ErrorReporter::new(source.clone());

    assert_eq!(reporter.source, source);
    assert!(reporter.file_name.is_none());
  }

  #[test]
  fn test_error_reporter_with_file_name() {
    let source = "let x = 42".to_string();
    let reporter = ErrorReporter::new(source).with_file_name("test.lat".to_string());

    assert_eq!(reporter.file_name, Some("test.lat".to_string()));
  }

  #[test]
  fn test_error_summary_generation() {
    let source = "let x = 42".to_string();
    let reporter = ErrorReporter::new(source);

    let location = SourceLocation::new(1, 1, 0);
    let error = ParseError::new(ParseErrorKind::UnexpectedToken, location);
    let errors = vec![error];

    let summary = reporter.generate_summary(&errors);
    assert!(summary.contains("Found 1 parse error(s):"));
    assert!(summary.contains("Unexpected token"));
  }
}
