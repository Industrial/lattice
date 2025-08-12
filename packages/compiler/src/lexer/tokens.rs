//! Token definitions and source location tracking for the Lattice language lexer.
//!
//! This module provides the core data structures for representing lexical tokens
//! and their source locations in the Lattice language.

use logos::Logos;
use std::fmt;

/// Represents a source location in the Lattice source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceLocation {
  /// Line number (1-indexed)
  pub line: usize,
  /// Column number (1-indexed)
  pub column: usize,
  /// Byte offset from start of file
  pub offset: usize,
}

impl SourceLocation {
  /// Create a new source location
  pub fn new(line: usize, column: usize, offset: usize) -> Self {
    Self {
      line,
      column,
      offset,
    }
  }

  /// Create a source location at the beginning of a file
  pub fn start() -> Self {
    Self::new(1, 1, 0)
  }

  /// Advance the location by one character
  pub fn advance(&mut self, ch: char) {
    if ch == '\n' {
      self.line += 1;
      self.column = 1;
    } else {
      self.column += 1;
    }
    self.offset += ch.len_utf8();
  }

  /// Advance the location by a string
  pub fn advance_by(&mut self, s: &str) {
    for ch in s.chars() {
      self.advance(ch);
    }
  }
}

impl fmt::Display for SourceLocation {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}:{}", self.line, self.column)
  }
}

/// Represents a token in the Lattice language
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
  /// The type of token
  pub kind: TokenKind,
  /// The source text that produced this token
  pub text: String,
  /// The location where this token starts
  pub start: SourceLocation,
  /// The location where this token ends
  pub end: SourceLocation,
}

impl Token {
  /// Create a new token
  pub fn new(kind: TokenKind, text: String, start: SourceLocation, end: SourceLocation) -> Self {
    Self {
      kind,
      text,
      start,
      end,
    }
  }

  /// Get the length of the token text
  pub fn len(&self) -> usize {
    self.text.len()
  }

  /// Check if the token text is empty
  pub fn is_empty(&self) -> bool {
    self.text.is_empty()
  }

  /// Get the span of the token (start to end location)
  pub fn span(&self) -> (SourceLocation, SourceLocation) {
    (self.start, self.end)
  }
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}('{}')", self.kind, self.text)
  }
}

/// Represents the different types of tokens in the Lattice language
#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
  // Keywords
  #[token("fn")]
  Function,
  #[token("let")]
  Let,
  #[token("in")]
  In,
  #[token("if")]
  If,
  #[token("then")]
  Then,
  #[token("else")]
  Else,
  #[token("match")]
  Match,
  #[token("with")]
  With,
  #[token("type")]
  Type,
  #[token("effect")]
  Effect,
  #[token("handle")]
  Handle,
  #[token("resume")]
  Resume,
  #[token("do")]
  Do,
  #[token("return")]
  Return,
  #[token("true")]
  True,
  #[token("false")]
  False,

  // Identifiers
  #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
  Identifier,

  // Literals
  #[regex(r#""([^"\\]|\\.)*""#)]
  String,
  #[regex(r"'([^'\\]|\\.)'")]
  Char,
  #[regex(r"[0-9]+")]
  Integer,
  #[regex(r"[0-9]+\.[0-9]+")]
  Float,
  #[regex(r"[0-9]+[uif][0-9]*")]
  TypedNumber,

  // Operators
  #[token("+")]
  Plus,
  #[token("-")]
  Minus,
  #[token("*")]
  Star,
  #[token("/")]
  Slash,
  #[token("%")]
  Percent,
  #[token("**")]
  Power,
  #[token("==")]
  Equal,
  #[token("!=")]
  NotEqual,
  #[token("<")]
  Less,
  #[token("<=")]
  LessEqual,
  #[token(">")]
  Greater,
  #[token(">=")]
  GreaterEqual,
  #[token("&&")]
  And,
  #[token("||")]
  Or,
  #[token("!")]
  Not,
  #[token("=")]
  Assign,
  #[token("+=")]
  PlusAssign,
  #[token("-=")]
  MinusAssign,
  #[token("*=")]
  StarAssign,
  #[token("/=")]
  SlashAssign,
  #[token("%=")]
  PercentAssign,
  #[token("**=")]
  PowerAssign,
  #[token("&")]
  BitwiseAnd,
  #[token("|")]
  BitwiseOr,
  #[token("^")]
  BitwiseXor,
  #[token("<<")]
  LeftShift,
  #[token(">>")]
  RightShift,
  #[token("~")]
  BitwiseNot,

  // Delimiters
  #[token("(")]
  LeftParen,
  #[token(")")]
  RightParen,
  #[token("[")]
  LeftBracket,
  #[token("]")]
  RightBracket,
  #[token("{")]
  LeftBrace,
  #[token("}")]
  RightBrace,
  #[token(",")]
  Comma,
  #[token(";")]
  Semicolon,
  #[token(":")]
  Colon,
  #[token(".")]
  Dot,
  #[token("->")]
  Arrow,
  #[token("=>")]
  FatArrow,
  #[token("_")]
  Underscore,

  // Comments
  #[regex(r"//.*")]
  LineComment,
  #[regex(r"/\*([^*]|\*[^/])*\*/")]
  BlockComment,
  #[regex(r"///.*")]
  DocComment,
  #[regex(r"/\*\*([^*]|\*[^/])*\*/")]
  DocBlockComment,

  // Whitespace
  #[regex(r"[ \t\r\n]+")]
  Whitespace,

  // Special tokens
  #[token("@")]
  At,
  #[token("#")]
  Hash,
  #[token("$")]
  Dollar,
  #[token("?")]
  Question,
  #[token("\\")]
  Backslash,

  // Error tokens
  InvalidToken,

  // End of file
  #[end]
  EndOfFile,
}

impl fmt::Display for TokenKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TokenKind::Function => write!(f, "Function"),
      TokenKind::Let => write!(f, "Let"),
      TokenKind::In => write!(f, "In"),
      TokenKind::If => write!(f, "If"),
      TokenKind::Then => write!(f, "Then"),
      TokenKind::Else => write!(f, "Else"),
      TokenKind::Match => write!(f, "Match"),
      TokenKind::With => write!(f, "With"),
      TokenKind::Type => write!(f, "Type"),
      TokenKind::Effect => write!(f, "Effect"),
      TokenKind::Handle => write!(f, "Handle"),
      TokenKind::Resume => write!(f, "Resume"),
      TokenKind::Do => write!(f, "Do"),
      TokenKind::Return => write!(f, "Return"),
      TokenKind::True => write!(f, "True"),
      TokenKind::False => write!(f, "False"),
      TokenKind::Identifier => write!(f, "Identifier"),
      TokenKind::String => write!(f, "String"),
      TokenKind::Char => write!(f, "Char"),
      TokenKind::Integer => write!(f, "Integer"),
      TokenKind::Float => write!(f, "Float"),
      TokenKind::TypedNumber => write!(f, "TypedNumber"),
      TokenKind::Plus => write!(f, "Plus"),
      TokenKind::Minus => write!(f, "Minus"),
      TokenKind::Star => write!(f, "Star"),
      TokenKind::Slash => write!(f, "Slash"),
      TokenKind::Percent => write!(f, "Percent"),
      TokenKind::Power => write!(f, "Power"),
      TokenKind::Equal => write!(f, "Equal"),
      TokenKind::NotEqual => write!(f, "NotEqual"),
      TokenKind::Less => write!(f, "Less"),
      TokenKind::LessEqual => write!(f, "LessEqual"),
      TokenKind::Greater => write!(f, "Greater"),
      TokenKind::GreaterEqual => write!(f, "GreaterEqual"),
      TokenKind::And => write!(f, "And"),
      TokenKind::Or => write!(f, "Or"),
      TokenKind::Not => write!(f, "Not"),
      TokenKind::Assign => write!(f, "Assign"),
      TokenKind::PlusAssign => write!(f, "PlusAssign"),
      TokenKind::MinusAssign => write!(f, "MinusAssign"),
      TokenKind::StarAssign => write!(f, "StarAssign"),
      TokenKind::SlashAssign => write!(f, "SlashAssign"),
      TokenKind::PercentAssign => write!(f, "PercentAssign"),
      TokenKind::PowerAssign => write!(f, "PowerAssign"),
      TokenKind::BitwiseAnd => write!(f, "BitwiseAnd"),
      TokenKind::BitwiseOr => write!(f, "BitwiseOr"),
      TokenKind::BitwiseXor => write!(f, "BitwiseXor"),
      TokenKind::LeftShift => write!(f, "LeftShift"),
      TokenKind::RightShift => write!(f, "RightShift"),
      TokenKind::BitwiseNot => write!(f, "BitwiseNot"),
      TokenKind::LeftParen => write!(f, "LeftParen"),
      TokenKind::RightParen => write!(f, "RightParen"),
      TokenKind::LeftBracket => write!(f, "LeftBracket"),
      TokenKind::RightBracket => write!(f, "RightBracket"),
      TokenKind::LeftBrace => write!(f, "LeftBrace"),
      TokenKind::RightBrace => write!(f, "RightBrace"),
      TokenKind::Comma => write!(f, "Comma"),
      TokenKind::Semicolon => write!(f, "Semicolon"),
      TokenKind::Colon => write!(f, "Colon"),
      TokenKind::Dot => write!(f, "Dot"),
      TokenKind::Arrow => write!(f, "Arrow"),
      TokenKind::FatArrow => write!(f, "FatArrow"),
      TokenKind::Underscore => write!(f, "Underscore"),
      TokenKind::LineComment => write!(f, "LineComment"),
      TokenKind::BlockComment => write!(f, "BlockComment"),
      TokenKind::DocComment => write!(f, "DocComment"),
      TokenKind::DocBlockComment => write!(f, "DocBlockComment"),
      TokenKind::Whitespace => write!(f, "Whitespace"),
      TokenKind::At => write!(f, "At"),
      TokenKind::Hash => write!(f, "Hash"),
      TokenKind::Dollar => write!(f, "Dollar"),
      TokenKind::Question => write!(f, "Question"),
      TokenKind::Backslash => write!(f, "Backslash"),
      TokenKind::InvalidToken => write!(f, "InvalidToken"),
      TokenKind::EndOfFile => write!(f, "EndOfFile"),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_source_location_creation() {
    let loc = SourceLocation::new(5, 10, 100);
    assert_eq!(loc.line, 5);
    assert_eq!(loc.column, 10);
    assert_eq!(loc.offset, 100);
  }

  #[test]
  fn test_source_location_start() {
    let loc = SourceLocation::start();
    assert_eq!(loc.line, 1);
    assert_eq!(loc.column, 1);
    assert_eq!(loc.offset, 0);
  }

  #[test]
  fn test_source_location_advance() {
    let mut loc = SourceLocation::start();
    loc.advance('a');
    assert_eq!(loc.line, 1);
    assert_eq!(loc.column, 2);
    assert_eq!(loc.offset, 1);

    loc.advance('\n');
    assert_eq!(loc.line, 2);
    assert_eq!(loc.column, 1);
    assert_eq!(loc.offset, 2);
  }

  #[test]
  fn test_source_location_advance_by() {
    let mut loc = SourceLocation::start();
    loc.advance_by("hello\nworld");
    assert_eq!(loc.line, 2);
    assert_eq!(loc.column, 6);
    assert_eq!(loc.offset, 11);
  }

  #[test]
  fn test_source_location_display() {
    let loc = SourceLocation::new(5, 10, 100);
    assert_eq!(loc.to_string(), "5:10");
  }

  #[test]
  fn test_token_creation() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 4, 3);
    let token = Token::new(TokenKind::Let, "let".to_string(), start, end);

    assert_eq!(token.kind, TokenKind::Let);
    assert_eq!(token.text, "let");
    assert_eq!(token.start, start);
    assert_eq!(token.end, end);
  }

  #[test]
  fn test_token_span() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 4, 3);
    let token = Token::new(TokenKind::Let, "let".to_string(), start, end);

    let (span_start, span_end) = token.span();
    assert_eq!(span_start, start);
    assert_eq!(span_end, end);
  }

  #[test]
  fn test_token_display() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 4, 3);
    let token = Token::new(TokenKind::Let, "let".to_string(), start, end);

    assert_eq!(token.to_string(), "Let('let')");
  }

  #[test]
  fn test_token_kind_display() {
    assert_eq!(TokenKind::Let.to_string(), "Let");
    assert_eq!(TokenKind::Plus.to_string(), "Plus");
    assert_eq!(TokenKind::Identifier.to_string(), "Identifier");
  }
}
