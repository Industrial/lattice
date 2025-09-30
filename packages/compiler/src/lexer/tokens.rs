//! Token definitions and source location tracking for the Lattice language lexer.
//!
//! This module provides the core data structures for representing lexical tokens
//! and their source locations in the Lattice language.

use chumsky::Span;
use logos::Logos;
use std::fmt;

/// Represents a source location in the Lattice source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

impl Span for Token {
  type Context = ();
  type Offset = usize;

  fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
    // This is a placeholder implementation - we'll need to create a proper Token
    // from the range, but for now we'll create a dummy token
    Token {
      kind: TokenKind::Identifier,
      text: String::new(),
      start: SourceLocation::new(1, 1, range.start),
      end: SourceLocation::new(1, 1, range.end),
    }
  }

  fn context(&self) -> Self::Context {
    ()
  }

  fn start(&self) -> Self::Offset {
    self.start.offset
  }

  fn end(&self) -> Self::Offset {
    self.end.offset
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
  #[token("as")]
  As,

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
      TokenKind::As => write!(f, "As"),
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

  // ============================================================================
  // SourceLocation Tests
  // ============================================================================

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
  fn test_source_location_advance_by_empty_string() {
    let mut loc = SourceLocation::new(5, 10, 100);
    let original = loc;
    loc.advance_by("");
    assert_eq!(loc, original);
  }

  #[test]
  fn test_source_location_advance_by_multi_line() {
    let mut loc = SourceLocation::start();
    loc.advance_by("line1\nline2\nline3");
    assert_eq!(loc.line, 3);
    assert_eq!(loc.column, 6);
    assert_eq!(loc.offset, 17);
  }

  #[test]
  fn test_source_location_advance_by_multi_byte_chars() {
    let mut loc = SourceLocation::start();
    // Test with multi-byte characters like emoji
    loc.advance_by("a🚀b");
    assert_eq!(loc.line, 1);
    assert_eq!(loc.column, 4); // a + 🚀 + b = 3 characters, starting from column 1
    assert_eq!(loc.offset, 6); // a(1) + 🚀(4) + b(1) = 6 bytes
  }

  #[test]
  fn test_source_location_display() {
    let loc = SourceLocation::new(5, 10, 100);
    assert_eq!(loc.to_string(), "5:10");
  }

  // ============================================================================
  // SourceLocation Trait Tests
  // ============================================================================

  #[test]
  fn test_source_location_clone() {
    let loc = SourceLocation::new(5, 10, 100);
    let cloned = loc.clone();
    assert_eq!(loc, cloned);
  }

  #[test]
  fn test_source_location_copy() {
    let loc = SourceLocation::new(5, 10, 100);
    let copied = loc; // This should work because SourceLocation is Copy
    assert_eq!(loc, copied);
  }

  #[test]
  fn test_source_location_partial_eq() {
    let loc1 = SourceLocation::new(5, 10, 100);
    let loc2 = SourceLocation::new(5, 10, 100);
    let loc3 = SourceLocation::new(5, 11, 100);

    assert_eq!(loc1, loc2);
    assert_ne!(loc1, loc3);
  }

  #[test]
  fn test_source_location_partial_ord() {
    let loc1 = SourceLocation::new(1, 1, 0);
    let loc2 = SourceLocation::new(1, 2, 1);
    let loc3 = SourceLocation::new(2, 1, 10);

    assert!(loc1 < loc2);
    assert!(loc2 < loc3);
    assert!(loc1 < loc3);
  }

  #[test]
  fn test_source_location_ord() {
    let loc1 = SourceLocation::new(1, 1, 0);
    let loc2 = SourceLocation::new(1, 2, 1);
    let loc3 = SourceLocation::new(2, 1, 10);

    assert_eq!(loc1.cmp(&loc2), std::cmp::Ordering::Less);
    assert_eq!(loc2.cmp(&loc3), std::cmp::Ordering::Less);
    assert_eq!(loc1.cmp(&loc3), std::cmp::Ordering::Less);
    assert_eq!(loc1.cmp(&loc1), std::cmp::Ordering::Equal);
  }

  #[test]
  fn test_source_location_hash() {
    use std::collections::HashMap;

    let mut map = HashMap::new();
    let loc1 = SourceLocation::new(5, 10, 100);
    let loc2 = SourceLocation::new(5, 10, 100);

    map.insert(loc1, "value1");
    assert_eq!(map.get(&loc2), Some(&"value1"));
  }

  // ============================================================================
  // Token Tests
  // ============================================================================

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
  fn test_token_len() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 4, 3);
    let token = Token::new(TokenKind::Let, "let".to_string(), start, end);

    assert_eq!(token.len(), 3);

    let empty_token = Token::new(TokenKind::Identifier, "".to_string(), start, end);
    assert_eq!(empty_token.len(), 0);
  }

  #[test]
  fn test_token_is_empty() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 4, 3);

    let token = Token::new(TokenKind::Let, "let".to_string(), start, end);
    assert!(!token.is_empty());

    let empty_token = Token::new(TokenKind::Identifier, "".to_string(), start, end);
    assert!(empty_token.is_empty());
  }

  #[test]
  fn test_token_display() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 4, 3);
    let token = Token::new(TokenKind::Let, "let".to_string(), start, end);

    assert_eq!(token.to_string(), "Let('let')");
  }

  #[test]
  fn test_token_display_with_empty_text() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 1, 0);
    let token = Token::new(TokenKind::Identifier, "".to_string(), start, end);

    assert_eq!(token.to_string(), "Identifier('')");
  }

  #[test]
  fn test_token_display_with_special_characters() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 3, 2);
    let token = Token::new(TokenKind::String, "\"hello\"".to_string(), start, end);

    assert_eq!(token.to_string(), "String('\"hello\"')");
  }

  // ============================================================================
  // Token Trait Tests
  // ============================================================================

  #[test]
  fn test_token_clone() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 4, 3);
    let token = Token::new(TokenKind::Let, "let".to_string(), start, end);
    let cloned = token.clone();

    assert_eq!(token, cloned);
    assert_eq!(token.text, cloned.text);
    assert_eq!(token.start, cloned.start);
    assert_eq!(token.end, cloned.end);
  }

  #[test]
  fn test_token_partial_eq() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 4, 3);

    let token1 = Token::new(TokenKind::Let, "let".to_string(), start, end);
    let token2 = Token::new(TokenKind::Let, "let".to_string(), start, end);
    let token3 = Token::new(TokenKind::Let, "var".to_string(), start, end);

    assert_eq!(token1, token2);
    assert_ne!(token1, token3);
  }

  #[test]
  fn test_token_eq() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 4, 3);

    let token1 = Token::new(TokenKind::Let, "let".to_string(), start, end);
    let token2 = Token::new(TokenKind::Let, "let".to_string(), start, end);

    assert!(token1 == token2);
    assert!(!(token1 != token2));
  }

  #[test]
  fn test_token_hash() {
    use std::collections::HashMap;

    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 4, 3);

    let mut map = HashMap::new();
    let token1 = Token::new(TokenKind::Let, "let".to_string(), start, end);
    let token2 = Token::new(TokenKind::Let, "let".to_string(), start, end);

    map.insert(token1, "value1");
    assert_eq!(map.get(&token2), Some(&"value1"));
  }

  // ============================================================================
  // TokenKind Display Tests
  // ============================================================================

  #[test]
  fn test_token_kind_display_keywords() {
    assert_eq!(TokenKind::Function.to_string(), "Function");
    assert_eq!(TokenKind::Let.to_string(), "Let");
    assert_eq!(TokenKind::In.to_string(), "In");
    assert_eq!(TokenKind::If.to_string(), "If");
    assert_eq!(TokenKind::Then.to_string(), "Then");
    assert_eq!(TokenKind::Else.to_string(), "Else");
    assert_eq!(TokenKind::Match.to_string(), "Match");
    assert_eq!(TokenKind::With.to_string(), "With");
    assert_eq!(TokenKind::Type.to_string(), "Type");
    assert_eq!(TokenKind::Effect.to_string(), "Effect");
    assert_eq!(TokenKind::Handle.to_string(), "Handle");
    assert_eq!(TokenKind::Resume.to_string(), "Resume");
    assert_eq!(TokenKind::Do.to_string(), "Do");
    assert_eq!(TokenKind::Return.to_string(), "Return");
    assert_eq!(TokenKind::True.to_string(), "True");
    assert_eq!(TokenKind::False.to_string(), "False");
  }

  #[test]
  fn test_token_kind_display_identifiers_and_literals() {
    assert_eq!(TokenKind::Identifier.to_string(), "Identifier");
    assert_eq!(TokenKind::String.to_string(), "String");
    assert_eq!(TokenKind::Char.to_string(), "Char");
    assert_eq!(TokenKind::Integer.to_string(), "Integer");
    assert_eq!(TokenKind::Float.to_string(), "Float");
    assert_eq!(TokenKind::TypedNumber.to_string(), "TypedNumber");
  }

  #[test]
  fn test_token_kind_display_operators() {
    assert_eq!(TokenKind::Plus.to_string(), "Plus");
    assert_eq!(TokenKind::Minus.to_string(), "Minus");
    assert_eq!(TokenKind::Star.to_string(), "Star");
    assert_eq!(TokenKind::Slash.to_string(), "Slash");
    assert_eq!(TokenKind::Percent.to_string(), "Percent");
    assert_eq!(TokenKind::Power.to_string(), "Power");
    assert_eq!(TokenKind::Equal.to_string(), "Equal");
    assert_eq!(TokenKind::NotEqual.to_string(), "NotEqual");
    assert_eq!(TokenKind::Less.to_string(), "Less");
    assert_eq!(TokenKind::LessEqual.to_string(), "LessEqual");
    assert_eq!(TokenKind::Greater.to_string(), "Greater");
    assert_eq!(TokenKind::GreaterEqual.to_string(), "GreaterEqual");
    assert_eq!(TokenKind::And.to_string(), "And");
    assert_eq!(TokenKind::Or.to_string(), "Or");
    assert_eq!(TokenKind::Not.to_string(), "Not");
    assert_eq!(TokenKind::Assign.to_string(), "Assign");
    assert_eq!(TokenKind::PlusAssign.to_string(), "PlusAssign");
    assert_eq!(TokenKind::MinusAssign.to_string(), "MinusAssign");
    assert_eq!(TokenKind::StarAssign.to_string(), "StarAssign");
    assert_eq!(TokenKind::SlashAssign.to_string(), "SlashAssign");
    assert_eq!(TokenKind::PercentAssign.to_string(), "PercentAssign");
    assert_eq!(TokenKind::PowerAssign.to_string(), "PowerAssign");
    assert_eq!(TokenKind::BitwiseAnd.to_string(), "BitwiseAnd");
    assert_eq!(TokenKind::BitwiseOr.to_string(), "BitwiseOr");
    assert_eq!(TokenKind::BitwiseXor.to_string(), "BitwiseXor");
    assert_eq!(TokenKind::LeftShift.to_string(), "LeftShift");
    assert_eq!(TokenKind::RightShift.to_string(), "RightShift");
    assert_eq!(TokenKind::BitwiseNot.to_string(), "BitwiseNot");
  }

  #[test]
  fn test_token_kind_display_delimiters() {
    assert_eq!(TokenKind::LeftParen.to_string(), "LeftParen");
    assert_eq!(TokenKind::RightParen.to_string(), "RightParen");
    assert_eq!(TokenKind::LeftBracket.to_string(), "LeftBracket");
    assert_eq!(TokenKind::RightBracket.to_string(), "RightBracket");
    assert_eq!(TokenKind::LeftBrace.to_string(), "LeftBrace");
    assert_eq!(TokenKind::RightBrace.to_string(), "RightBrace");
    assert_eq!(TokenKind::Comma.to_string(), "Comma");
    assert_eq!(TokenKind::Semicolon.to_string(), "Semicolon");
    assert_eq!(TokenKind::Colon.to_string(), "Colon");
    assert_eq!(TokenKind::Dot.to_string(), "Dot");
    assert_eq!(TokenKind::Arrow.to_string(), "Arrow");
    assert_eq!(TokenKind::FatArrow.to_string(), "FatArrow");
    assert_eq!(TokenKind::Underscore.to_string(), "Underscore");
    assert_eq!(TokenKind::As.to_string(), "As");
  }

  #[test]
  fn test_token_kind_display_comments() {
    assert_eq!(TokenKind::LineComment.to_string(), "LineComment");
    assert_eq!(TokenKind::BlockComment.to_string(), "BlockComment");
    assert_eq!(TokenKind::DocComment.to_string(), "DocComment");
    assert_eq!(TokenKind::DocBlockComment.to_string(), "DocBlockComment");
  }

  #[test]
  fn test_token_kind_display_whitespace_and_special() {
    assert_eq!(TokenKind::Whitespace.to_string(), "Whitespace");
    assert_eq!(TokenKind::At.to_string(), "At");
    assert_eq!(TokenKind::Hash.to_string(), "Hash");
    assert_eq!(TokenKind::Dollar.to_string(), "Dollar");
    assert_eq!(TokenKind::Question.to_string(), "Question");
    assert_eq!(TokenKind::Backslash.to_string(), "Backslash");
  }

  #[test]
  fn test_token_kind_display_error_and_eof() {
    assert_eq!(TokenKind::InvalidToken.to_string(), "InvalidToken");
    assert_eq!(TokenKind::EndOfFile.to_string(), "EndOfFile");
  }

  // ============================================================================
  // TokenKind Trait Tests
  // ============================================================================

  #[test]
  fn test_token_kind_clone() {
    let kind = TokenKind::Let;
    let cloned = kind.clone();
    assert_eq!(kind, cloned);
  }

  #[test]
  fn test_token_kind_partial_eq() {
    assert_eq!(TokenKind::Let, TokenKind::Let);
    assert_ne!(TokenKind::Let, TokenKind::Function);
  }

  #[test]
  fn test_token_kind_eq() {
    assert!(TokenKind::Let == TokenKind::Let);
    assert!(!(TokenKind::Let == TokenKind::Function));
  }

  #[test]
  fn test_token_kind_hash() {
    use std::collections::HashMap;

    let mut map = HashMap::new();
    map.insert(TokenKind::Let, "let keyword");
    map.insert(TokenKind::Function, "function keyword");

    assert_eq!(map.get(&TokenKind::Let), Some(&"let keyword"));
    assert_eq!(map.get(&TokenKind::Function), Some(&"function keyword"));
  }

  // ============================================================================
  // Edge Cases and Integration Tests
  // ============================================================================

  #[test]
  fn test_source_location_advance_with_carriage_return() {
    let mut loc = SourceLocation::start();
    loc.advance('\r');
    // \r should not change line number, only column and offset
    assert_eq!(loc.line, 1);
    assert_eq!(loc.column, 2);
    assert_eq!(loc.offset, 1);
  }

  #[test]
  fn test_source_location_advance_with_tab() {
    let mut loc = SourceLocation::start();
    loc.advance('\t');
    // Tab should increment column by 1 (not by tab width)
    assert_eq!(loc.line, 1);
    assert_eq!(loc.column, 2);
    assert_eq!(loc.offset, 1);
  }

  #[test]
  fn test_token_with_zero_length() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 1, 0);
    let token = Token::new(TokenKind::Identifier, "".to_string(), start, end);

    assert_eq!(token.len(), 0);
    assert!(token.is_empty());
    assert_eq!(token.start, token.end);
  }

  #[test]
  fn test_token_with_multi_line_span() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(3, 5, 25);
    let token = Token::new(
      TokenKind::String,
      "multi\nline\nstring".to_string(),
      start,
      end,
    );

    assert_eq!(token.start.line, 1);
    assert_eq!(token.end.line, 3);
    assert_eq!(token.start.column, 1);
    assert_eq!(token.end.column, 5);
  }

  #[test]
  fn test_source_location_ordering_edge_cases() {
    let loc1 = SourceLocation::new(1, 1, 0);
    let loc2 = SourceLocation::new(1, 1, 1);
    let loc3 = SourceLocation::new(1, 2, 0);
    let loc4 = SourceLocation::new(2, 1, 0);

    // Test ordering by line first, then column, then offset
    assert!(loc1 < loc2);
    assert!(loc2 < loc3);
    assert!(loc3 < loc4);

    // Test that offset doesn't affect line/column ordering
    let loc5 = SourceLocation::new(1, 1, 100);
    assert!(loc1 < loc5);
  }

  // ============================================================================
  // Additional Edge Cases and Boundary Tests
  // ============================================================================

  #[test]
  fn test_source_location_advance_with_zero_values() {
    let mut loc = SourceLocation::new(0, 0, 0);
    loc.advance('a');
    assert_eq!(loc.line, 0);
    assert_eq!(loc.column, 1);
    assert_eq!(loc.offset, 1);
  }

  #[test]
  fn test_source_location_advance_with_unicode_characters() {
    let mut loc = SourceLocation::start();
    // Test with various Unicode characters
    loc.advance('ñ'); // 2 bytes
    assert_eq!(loc.line, 1);
    assert_eq!(loc.column, 2);
    assert_eq!(loc.offset, 2);

    loc.advance('🚀'); // 4 bytes
    assert_eq!(loc.line, 1);
    assert_eq!(loc.column, 3);
    assert_eq!(loc.offset, 6);

    loc.advance('字'); // 3 bytes
    assert_eq!(loc.line, 1);
    assert_eq!(loc.column, 4);
    assert_eq!(loc.offset, 9);
  }

  #[test]
  fn test_source_location_advance_by_with_mixed_content() {
    let mut loc = SourceLocation::start();
    loc.advance_by("hello\nworld\r\nunix\n");
    assert_eq!(loc.line, 4);
    assert_eq!(loc.column, 1);
    assert_eq!(loc.offset, 18);
  }

  #[test]
  fn test_source_location_equality_and_ordering() {
    let loc1 = SourceLocation::new(1, 1, 0);
    let loc2 = SourceLocation::new(1, 1, 0);
    let loc3 = SourceLocation::new(1, 1, 1);

    // Test equality
    assert_eq!(loc1, loc2);
    assert_eq!(loc1.cmp(&loc2), std::cmp::Ordering::Equal);

    // Test ordering
    assert!(loc1 <= loc2);
    assert!(loc1 >= loc2);
    assert!(loc1 < loc3);
    assert!(loc3 > loc1);
    assert!(loc1 <= loc3);
    assert!(loc3 >= loc1);
  }

  #[test]
  fn test_token_with_very_long_text() {
    let start = SourceLocation::new(1, 1, 0);
    let long_text = "a".repeat(1000);
    let end = SourceLocation::new(1, 1001, 1000);
    let token = Token::new(TokenKind::String, long_text.clone(), start, end);

    assert_eq!(token.len(), 1000);
    assert!(!token.is_empty());
    assert_eq!(token.text, long_text);
  }

  #[test]
  fn test_token_with_special_characters_in_display() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 7, 6);

    // Test with quotes in the text
    let token1 = Token::new(TokenKind::String, "hello\"world".to_string(), start, end);
    assert_eq!(token1.to_string(), "String('hello\"world')");

    // Test with backslashes
    let token2 = Token::new(TokenKind::String, "hello\\world".to_string(), start, end);
    assert_eq!(token2.to_string(), "String('hello\\world')");

    // Test with newlines
    let token3 = Token::new(TokenKind::String, "hello\nworld".to_string(), start, end);
    assert_eq!(token3.to_string(), "String('hello\nworld')");
  }

  #[test]
  fn test_source_location_boundary_conditions() {
    // Test with maximum values
    let max_loc = SourceLocation::new(usize::MAX, usize::MAX, usize::MAX);
    assert_eq!(max_loc.line, usize::MAX);
    assert_eq!(max_loc.column, usize::MAX);
    assert_eq!(max_loc.offset, usize::MAX);

    // Test display with large numbers
    assert_eq!(
      max_loc.to_string(),
      format!("{}:{}", usize::MAX, usize::MAX)
    );
  }

  #[test]
  fn test_token_span_with_different_locations() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(5, 10, 100);
    let token = Token::new(TokenKind::Identifier, "test".to_string(), start, end);

    let (span_start, span_end) = token.span();
    assert_eq!(span_start, start);
    assert_eq!(span_end, end);
    assert_ne!(span_start, span_end);
  }

  #[test]
  fn test_source_location_advance_by_single_character() {
    let mut loc = SourceLocation::start();
    loc.advance_by("a");
    assert_eq!(loc.line, 1);
    assert_eq!(loc.column, 2);
    assert_eq!(loc.offset, 1);
  }

  #[test]
  fn test_source_location_advance_by_only_newlines() {
    let mut loc = SourceLocation::start();
    loc.advance_by("\n\n\n");
    assert_eq!(loc.line, 4);
    assert_eq!(loc.column, 1);
    assert_eq!(loc.offset, 3);
  }

  #[test]
  fn test_token_kind_display_comprehensive() {
    // Test all TokenKind variants to ensure Display implementation is complete
    let all_variants = vec![
      TokenKind::Function,
      TokenKind::Let,
      TokenKind::In,
      TokenKind::If,
      TokenKind::Then,
      TokenKind::Else,
      TokenKind::Match,
      TokenKind::With,
      TokenKind::Type,
      TokenKind::Effect,
      TokenKind::Handle,
      TokenKind::Resume,
      TokenKind::Do,
      TokenKind::Return,
      TokenKind::True,
      TokenKind::False,
      TokenKind::Identifier,
      TokenKind::String,
      TokenKind::Char,
      TokenKind::Integer,
      TokenKind::Float,
      TokenKind::TypedNumber,
      TokenKind::Plus,
      TokenKind::Minus,
      TokenKind::Star,
      TokenKind::Slash,
      TokenKind::Percent,
      TokenKind::Power,
      TokenKind::Equal,
      TokenKind::NotEqual,
      TokenKind::Less,
      TokenKind::LessEqual,
      TokenKind::Greater,
      TokenKind::GreaterEqual,
      TokenKind::And,
      TokenKind::Or,
      TokenKind::Not,
      TokenKind::Assign,
      TokenKind::PlusAssign,
      TokenKind::MinusAssign,
      TokenKind::StarAssign,
      TokenKind::SlashAssign,
      TokenKind::PercentAssign,
      TokenKind::PowerAssign,
      TokenKind::BitwiseAnd,
      TokenKind::BitwiseOr,
      TokenKind::BitwiseXor,
      TokenKind::LeftShift,
      TokenKind::RightShift,
      TokenKind::BitwiseNot,
      TokenKind::LeftParen,
      TokenKind::RightParen,
      TokenKind::LeftBracket,
      TokenKind::RightBracket,
      TokenKind::LeftBrace,
      TokenKind::RightBrace,
      TokenKind::Comma,
      TokenKind::Semicolon,
      TokenKind::Colon,
      TokenKind::Dot,
      TokenKind::Arrow,
      TokenKind::FatArrow,
      TokenKind::Underscore,
      TokenKind::As,
      TokenKind::LineComment,
      TokenKind::BlockComment,
      TokenKind::DocComment,
      TokenKind::DocBlockComment,
      TokenKind::Whitespace,
      TokenKind::At,
      TokenKind::Hash,
      TokenKind::Dollar,
      TokenKind::Question,
      TokenKind::Backslash,
      TokenKind::InvalidToken,
      TokenKind::EndOfFile,
    ];

    // Ensure all variants can be converted to string without panicking
    for variant in all_variants {
      let _ = variant.to_string();
    }
  }

  #[test]
  fn test_source_location_hash_consistency() {
    use std::collections::HashMap;

    let mut map = HashMap::new();
    let loc1 = SourceLocation::new(1, 1, 0);
    let loc2 = SourceLocation::new(1, 1, 0);
    let loc3 = SourceLocation::new(1, 2, 0);

    map.insert(loc1, "first");

    // Same location should retrieve the same value
    assert_eq!(map.get(&loc2), Some(&"first"));

    // Different location should not retrieve the value
    assert_eq!(map.get(&loc3), None);
  }

  #[test]
  fn test_token_hash_consistency() {
    use std::collections::HashMap;

    let mut map = HashMap::new();
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 4, 3);

    let token1 = Token::new(TokenKind::Let, "let".to_string(), start, end);
    let token2 = Token::new(TokenKind::Let, "let".to_string(), start, end);
    let token3 = Token::new(TokenKind::Let, "var".to_string(), start, end);

    map.insert(token1, "let keyword");

    // Same token should retrieve the same value
    assert_eq!(map.get(&token2), Some(&"let keyword"));

    // Different token should not retrieve the value
    assert_eq!(map.get(&token3), None);
  }
}
