//! Lexical Analysis Module for Lattice Language
//!
//! This module provides lexical analysis functionality for the Lattice functional programming language.
//! It includes token definitions, source location tracking, and error handling for the lexer.

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
    write!(f, "{}('{}') at {}", self.kind, self.text, self.start)
  }
}

/// Represents all possible token types in the Lattice language
#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
  // Keywords
  #[token("fn")]
  Fn,
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
  #[token("return")]
  Return,
  #[token("break")]
  Break,
  #[token("continue")]
  Continue,
  #[token("for")]
  For,
  #[token("while")]
  While,
  #[token("do")]
  Do,
  #[token("where")]
  Where,
  #[token("module")]
  Module,
  #[token("import")]
  Import,
  #[token("export")]
  Export,
  #[token("as")]
  As,
  #[token("pub")]
  Pub,
  #[token("priv")]
  Priv,
  #[token("const")]
  Const,
  #[token("static")]
  Static,
  #[token("mut")]
  Mut,
  #[token("ref")]
  Ref,
  #[token("move")]
  Move,
  #[token("copy")]
  Copy,
  #[token("clone")]
  Clone,
  #[token("drop")]
  Drop,
  #[token("unsafe")]
  Unsafe,
  #[token("extern")]
  Extern,
  #[token("crate")]
  Crate,
  #[token("super")]
  Super,
  #[token("self")]
  Self_,
  #[token("Self")]
  SelfType,
  #[token("use")]
  Use,
  #[token("mod")]
  Mod,
  #[token("struct")]
  Struct,
  #[token("enum")]
  Enum,
  #[token("union")]
  Union,
  #[token("trait")]
  Trait,
  #[token("impl")]
  Impl,
  #[token("default")]
  Default,
  #[token("final")]
  Final,
  #[token("virtual")]
  Virtual,
  #[token("override")]
  Override,
  #[token("abstract")]
  Abstract,
  #[token("sealed")]
  Sealed,
  #[token("open")]
  Open,

  // Literals
  #[regex(r#""([^"\\]|\\["\\/bfnrt]|\\u[0-9a-fA-F]{4})*""#)]
  String,
  #[regex(r"'([^'\\]|\\['\\/bfnrt]|\\u[0-9a-fA-F]{4})'")]
  Char,
  #[regex(r"[0-9]+", priority = 2)]
  Integer,
  #[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", priority = 2)]
  Float,
  #[regex(r"true|false", priority = 2)]
  Boolean,
  #[regex(r"[0-9]+[uif][0-9]*", priority = 2)]
  TypedNumber,

  // Identifiers
  #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", priority = 1)]
  Identifier,

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
  DoubleStar,
  #[token("//", priority = 3)]
  DoubleSlash,
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
  #[token("<=>")]
  Spaceship,
  #[token("&&")]
  And,
  #[token("||")]
  Or,
  #[token("!")]
  Not,
  #[token("&")]
  BitAnd,
  #[token("|", priority = 3)]
  BitOr,
  #[token("^")]
  BitXor,
  #[token("<<")]
  LeftShift,
  #[token(">>")]
  RightShift,
  #[token(">>>")]
  UnsignedRightShift,
  #[token("~")]
  BitNot,
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
  DoubleStarAssign,
  #[token("//=")]
  DoubleSlashAssign,
  #[token("&=")]
  BitAndAssign,
  #[token("|=")]
  BitOrAssign,
  #[token("^=")]
  BitXorAssign,
  #[token("<<=")]
  LeftShiftAssign,
  #[token(">>=")]
  RightShiftAssign,
  #[token(">>>=")]
  UnsignedRightShiftAssign,
  #[token("=>")]
  FatArrow,
  #[token("->")]
  ThinArrow,
  #[token("::")]
  DoubleColon,
  #[token("..")]
  DoubleDot,
  #[token("...")]
  TripleDot,
  #[token("..=")]
  DoubleDotEqual,
  #[token("?")]
  Question,
  #[token("@")]
  At,
  #[token("#")]
  Hash,
  #[token("$")]
  Dollar,
  #[token("`")]
  Backtick,
  #[token("\\")]
  Backslash,

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
  #[token("_")]
  Underscore,

  // Comments
  #[regex(r"///.*", priority = 6)]
  DocComment,
  #[regex(r"//.*", priority = 5)]
  LineComment,
  #[regex(r"/\*\*([^*]|\*[^/])*\*/")]
  DocBlockComment,
  #[regex(r"/\*([^*]|\*[^/])*\*/")]
  BlockComment,

  // Whitespace
  #[regex(r"[ \t\r\n\f]+")]
  Whitespace,

  // End of file
  #[regex(r"EOF")]
  Eof,
}

impl fmt::Display for TokenKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      // Keywords
      TokenKind::Fn => write!(f, "fn"),
      TokenKind::Let => write!(f, "let"),
      TokenKind::In => write!(f, "in"),
      TokenKind::If => write!(f, "if"),
      TokenKind::Then => write!(f, "then"),
      TokenKind::Else => write!(f, "else"),
      TokenKind::Match => write!(f, "match"),
      TokenKind::With => write!(f, "with"),
      TokenKind::Type => write!(f, "type"),
      TokenKind::Effect => write!(f, "effect"),
      TokenKind::Handle => write!(f, "handle"),
      TokenKind::Resume => write!(f, "resume"),
      TokenKind::Return => write!(f, "return"),
      TokenKind::Break => write!(f, "break"),
      TokenKind::Continue => write!(f, "continue"),
      TokenKind::For => write!(f, "for"),
      TokenKind::While => write!(f, "while"),
      TokenKind::Do => write!(f, "do"),
      TokenKind::Where => write!(f, "where"),
      TokenKind::Module => write!(f, "module"),
      TokenKind::Import => write!(f, "import"),
      TokenKind::Export => write!(f, "export"),
      TokenKind::As => write!(f, "as"),
      TokenKind::Pub => write!(f, "pub"),
      TokenKind::Priv => write!(f, "priv"),
      TokenKind::Const => write!(f, "const"),
      TokenKind::Static => write!(f, "static"),
      TokenKind::Mut => write!(f, "mut"),
      TokenKind::Ref => write!(f, "ref"),
      TokenKind::Move => write!(f, "move"),
      TokenKind::Copy => write!(f, "copy"),
      TokenKind::Clone => write!(f, "clone"),
      TokenKind::Drop => write!(f, "drop"),
      TokenKind::Unsafe => write!(f, "unsafe"),
      TokenKind::Extern => write!(f, "extern"),
      TokenKind::Crate => write!(f, "crate"),
      TokenKind::Super => write!(f, "super"),
      TokenKind::Self_ => write!(f, "self"),
      TokenKind::SelfType => write!(f, "Self"),
      TokenKind::Use => write!(f, "use"),
      TokenKind::Mod => write!(f, "mod"),
      TokenKind::Struct => write!(f, "struct"),
      TokenKind::Enum => write!(f, "enum"),
      TokenKind::Union => write!(f, "union"),
      TokenKind::Trait => write!(f, "trait"),
      TokenKind::Impl => write!(f, "impl"),
      TokenKind::Default => write!(f, "default"),
      TokenKind::Final => write!(f, "final"),
      TokenKind::Virtual => write!(f, "virtual"),
      TokenKind::Override => write!(f, "override"),
      TokenKind::Abstract => write!(f, "abstract"),
      TokenKind::Sealed => write!(f, "sealed"),
      TokenKind::Open => write!(f, "open"),

      // Literals
      TokenKind::String => write!(f, "string literal"),
      TokenKind::Char => write!(f, "character literal"),
      TokenKind::Integer => write!(f, "integer literal"),
      TokenKind::Float => write!(f, "float literal"),
      TokenKind::Boolean => write!(f, "boolean literal"),
      TokenKind::TypedNumber => write!(f, "typed number literal"),

      // Identifiers
      TokenKind::Identifier => write!(f, "identifier"),

      // Operators
      TokenKind::Plus => write!(f, "+"),
      TokenKind::Minus => write!(f, "-"),
      TokenKind::Star => write!(f, "*"),
      TokenKind::Slash => write!(f, "/"),
      TokenKind::Percent => write!(f, "%"),
      TokenKind::DoubleStar => write!(f, "**"),
      TokenKind::DoubleSlash => write!(f, "//"),
      TokenKind::Equal => write!(f, "=="),
      TokenKind::NotEqual => write!(f, "!="),
      TokenKind::Less => write!(f, "<"),
      TokenKind::LessEqual => write!(f, "<="),
      TokenKind::Greater => write!(f, ">"),
      TokenKind::GreaterEqual => write!(f, ">="),
      TokenKind::Spaceship => write!(f, "<=>"),
      TokenKind::And => write!(f, "&&"),
      TokenKind::Or => write!(f, "||"),
      TokenKind::Not => write!(f, "!"),
      TokenKind::BitAnd => write!(f, "&"),
      TokenKind::BitOr => write!(f, "|"),
      TokenKind::BitXor => write!(f, "^"),
      TokenKind::LeftShift => write!(f, "<<"),
      TokenKind::RightShift => write!(f, ">>"),
      TokenKind::UnsignedRightShift => write!(f, ">>>"),
      TokenKind::BitNot => write!(f, "~"),
      TokenKind::Assign => write!(f, "="),
      TokenKind::PlusAssign => write!(f, "+="),
      TokenKind::MinusAssign => write!(f, "-="),
      TokenKind::StarAssign => write!(f, "*="),
      TokenKind::SlashAssign => write!(f, "/="),
      TokenKind::PercentAssign => write!(f, "%="),
      TokenKind::DoubleStarAssign => write!(f, "**="),
      TokenKind::DoubleSlashAssign => write!(f, "//="),
      TokenKind::BitAndAssign => write!(f, "&="),
      TokenKind::BitOrAssign => write!(f, "|="),
      TokenKind::BitXorAssign => write!(f, "^="),
      TokenKind::LeftShiftAssign => write!(f, "<<="),
      TokenKind::RightShiftAssign => write!(f, ">>="),
      TokenKind::UnsignedRightShiftAssign => write!(f, ">>>="),
      TokenKind::FatArrow => write!(f, "=>"),
      TokenKind::ThinArrow => write!(f, "->"),
      TokenKind::DoubleColon => write!(f, "::"),
      TokenKind::DoubleDot => write!(f, ".."),
      TokenKind::TripleDot => write!(f, "..."),
      TokenKind::DoubleDotEqual => write!(f, "..="),
      TokenKind::Question => write!(f, "?"),
      TokenKind::At => write!(f, "@"),
      TokenKind::Hash => write!(f, "#"),
      TokenKind::Dollar => write!(f, "$"),
      TokenKind::Backtick => write!(f, "`"),
      TokenKind::Backslash => write!(f, "\\"),

      // Delimiters
      TokenKind::LeftParen => write!(f, "("),
      TokenKind::RightParen => write!(f, ")"),
      TokenKind::LeftBracket => write!(f, "["),
      TokenKind::RightBracket => write!(f, "]"),
      TokenKind::LeftBrace => write!(f, "{{"),
      TokenKind::RightBrace => write!(f, "}}"),
      TokenKind::Comma => write!(f, ","),
      TokenKind::Semicolon => write!(f, ";"),
      TokenKind::Colon => write!(f, ":"),
      TokenKind::Dot => write!(f, "."),
      TokenKind::Underscore => write!(f, "_"),

      // Comments
      TokenKind::LineComment => write!(f, "line comment"),
      TokenKind::BlockComment => write!(f, "block comment"),
      TokenKind::DocComment => write!(f, "doc comment"),
      TokenKind::DocBlockComment => write!(f, "doc block comment"),

      // Whitespace
      TokenKind::Whitespace => write!(f, "whitespace"),

      // End of file
      TokenKind::Eof => write!(f, "EOF"),
    }
  }
}

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
  /// Invalid escape sequence
  InvalidEscapeSequence(String, SourceLocation),
  /// Invalid number format
  InvalidNumber(String, SourceLocation),
  /// Unexpected end of input
  UnexpectedEndOfInput(SourceLocation),
}

impl fmt::Display for LexerError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      LexerError::InvalidCharacter(ch, loc) => {
        write!(f, "Invalid character '{}' at {}", ch, loc)
      }
      LexerError::UnterminatedString(loc) => {
        write!(f, "Unterminated string literal at {}", loc)
      }
      LexerError::UnterminatedChar(loc) => {
        write!(f, "Unterminated character literal at {}", loc)
      }
      LexerError::UnterminatedBlockComment(loc) => {
        write!(f, "Unterminated block comment at {}", loc)
      }
      LexerError::InvalidEscapeSequence(seq, loc) => {
        write!(f, "Invalid escape sequence '{}' at {}", seq, loc)
      }
      LexerError::InvalidNumber(num, loc) => {
        write!(f, "Invalid number format '{}' at {}", num, loc)
      }
      LexerError::UnexpectedEndOfInput(loc) => {
        write!(f, "Unexpected end of input at {}", loc)
      }
    }
  }
}

impl std::error::Error for LexerError {}

/// Result type for lexer operations
pub type LexerResult<T> = Result<T, LexerError>;

/// A lexer for the Lattice language that converts source code into tokens
pub struct Lexer {
  /// The source code to tokenize
  source: String,
  /// Current source location
  current_location: SourceLocation,
}

impl Lexer {
  /// Create a new lexer for the given source code
  pub fn new(source: String) -> Self {
    Self {
      source,
      current_location: SourceLocation::start(),
    }
  }

  /// Create a new lexer from a string slice
  pub fn from_str(source: &str) -> Self {
    Self::new(source.to_string())
  }

  /// Get the source code being tokenized
  pub fn source(&self) -> &str {
    &self.source
  }

  /// Get the current source location
  pub fn current_location(&self) -> SourceLocation {
    self.current_location
  }

  /// Reset the lexer to the beginning
  pub fn reset(&mut self) {
    self.current_location = SourceLocation::start();
  }

  /// Tokenize the entire source code into a vector of tokens
  pub fn tokenize(&mut self) -> LexerResult<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut lexer = TokenKind::lexer(&self.source);
    let mut current_location = SourceLocation::start();

    while let Some(token_result) = lexer.next() {
      match token_result {
        Ok(kind) => {
          let start = current_location;
          let text = lexer.slice().to_string();

          // Advance the location by the token text
          current_location.advance_by(&text);
          let end = current_location;

          // Create the token
          let token = Token::new(kind.clone(), text, start, end);

          // Skip whitespace and comments for the main token stream
          if !matches!(
            kind,
            TokenKind::Whitespace
              | TokenKind::LineComment
              | TokenKind::BlockComment
              | TokenKind::DocComment
              | TokenKind::DocBlockComment
          ) {
            tokens.push(token);
          }
        }
        Err(_) => {
          // Handle logos errors - create an error token or skip
          continue;
        }
      }
    }

    self.current_location = current_location;
    Ok(tokens)
  }

  /// Tokenize including whitespace and comments
  pub fn tokenize_all(&mut self) -> LexerResult<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut lexer = TokenKind::lexer(&self.source);
    let mut current_location = SourceLocation::start();

    while let Some(token_result) = lexer.next() {
      match token_result {
        Ok(kind) => {
          let start = current_location;
          let text = lexer.slice().to_string();

          // Advance the location by the token text
          current_location.advance_by(&text);
          let end = current_location;

          // Create the token
          let token = Token::new(kind, text, start, end);
          tokens.push(token);
        }
        Err(_) => {
          // Handle logos errors - create an error token or skip
          continue;
        }
      }
    }

    self.current_location = current_location;
    Ok(tokens)
  }

  /// Peek at the next token without consuming it
  pub fn peek(&self) -> Option<TokenKind> {
    let mut lexer = TokenKind::lexer(&self.source);
    lexer.next().transpose().ok().flatten()
  }

  /// Check if the lexer has reached the end
  pub fn is_eof(&self) -> bool {
    // Since we don't track position during tokenization,
    // we'll consider it EOF if we've processed the entire source
    self.current_location.offset >= self.source.len()
  }

  /// Get the remaining source code
  pub fn remaining(&self) -> &str {
    // Since we don't track position during tokenization,
    // return empty string if we've processed everything
    if self.current_location.offset >= self.source.len() {
      ""
    } else {
      &self.source[self.current_location.offset..]
    }
  }
}

/// Create a lexer from a string slice
impl From<&str> for Lexer {
  fn from(source: &str) -> Self {
    Lexer::from_str(source)
  }
}

/// Create a lexer from a String
impl From<String> for Lexer {
  fn from(source: String) -> Self {
    Lexer::new(source)
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
    let end = SourceLocation::new(1, 3, 2);
    let token = Token::new(TokenKind::Identifier, "x".to_string(), start, end);

    assert_eq!(token.kind, TokenKind::Identifier);
    assert_eq!(token.text, "x");
    assert_eq!(token.start, start);
    assert_eq!(token.end, end);
  }

  #[test]
  fn test_token_span() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 3, 2);
    let token = Token::new(TokenKind::Identifier, "x".to_string(), start, end);

    let (span_start, span_end) = token.span();
    assert_eq!(span_start, start);
    assert_eq!(span_end, end);
  }

  #[test]
  fn test_token_display() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 3, 2);
    let token = Token::new(TokenKind::Identifier, "x".to_string(), start, end);

    let display = token.to_string();
    assert!(display.contains("identifier"));
    assert!(display.contains("x"));
    assert!(display.contains("1:1"));
  }

  #[test]
  fn test_token_kind_display() {
    assert_eq!(TokenKind::Fn.to_string(), "fn");
    assert_eq!(TokenKind::Let.to_string(), "let");
    assert_eq!(TokenKind::Plus.to_string(), "+");
    assert_eq!(TokenKind::Identifier.to_string(), "identifier");
  }

  #[test]
  fn test_lexer_error_display() {
    let loc = SourceLocation::new(1, 1, 0);
    let error = LexerError::InvalidCharacter('@', loc);
    assert_eq!(error.to_string(), "Invalid character '@' at 1:1");
  }

  // Lexer functionality tests
  #[test]
  fn test_lexer_creation() {
    let source = "let x = 42";
    let lexer = Lexer::from_str(source);
    assert_eq!(lexer.source(), source);
    assert_eq!(lexer.current_location(), SourceLocation::start());
  }

  #[test]
  fn test_lexer_from_string() {
    let source = String::from("fn add x y");
    let lexer = Lexer::from(source.clone());
    assert_eq!(lexer.source(), source);
  }

  #[test]
  fn test_lexer_tokenization() {
    let mut lexer = Lexer::from_str("let x = 42");
    let tokens = lexer.tokenize().unwrap();

    assert_eq!(tokens.len(), 4);
    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::Assign);
    assert_eq!(tokens[3].kind, TokenKind::Integer);
  }

  #[test]
  fn test_lexer_tokenization_with_whitespace() {
    let mut lexer = Lexer::from_str("  let   x   =   42  ");
    let tokens = lexer.tokenize().unwrap();

    assert_eq!(tokens.len(), 4);
    assert_eq!(tokens[0].kind, TokenKind::Let);
    assert_eq!(tokens[1].kind, TokenKind::Identifier);
    assert_eq!(tokens[2].kind, TokenKind::Assign);
    assert_eq!(tokens[3].kind, TokenKind::Integer);
  }

  #[test]
  fn test_lexer_tokenization_all() {
    let mut lexer = Lexer::from_str("let x = 42");
    let tokens = lexer.tokenize_all().unwrap();

    // Should include whitespace tokens
    assert!(tokens.len() >= 4);
    assert_eq!(tokens[0].kind, TokenKind::Let);
  }

  #[test]
  fn test_lexer_source_locations() {
    let mut lexer = Lexer::from_str("let x = 42");
    let tokens = lexer.tokenize().unwrap();

    // Check that locations are properly tracked
    assert_eq!(tokens[0].start.line, 1);
    assert_eq!(tokens[0].start.column, 1);
    assert_eq!(tokens[0].end.line, 1);
    assert_eq!(tokens[0].end.column, 4); // "let" is 3 chars + 1

    assert_eq!(tokens[1].start.line, 1);
    assert_eq!(tokens[1].start.column, 5); // After "let "
    assert_eq!(tokens[1].end.line, 1);
    assert_eq!(tokens[1].end.column, 6); // "x" is 1 char + 1
  }

  #[test]
  fn test_lexer_keywords() {
    let mut lexer = Lexer::from_str("fn let in if then else match with type effect handle resume");
    let tokens = lexer.tokenize().unwrap();

    let expected_keywords = vec![
      TokenKind::Fn,
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
    ];

    assert_eq!(tokens.len(), expected_keywords.len());
    for (i, expected) in expected_keywords.iter().enumerate() {
      assert_eq!(tokens[i].kind, *expected);
    }
  }

  #[test]
  fn test_lexer_operators() {
    let mut lexer = Lexer::from_str("+ - * / % == != < <= > >= && || ! & | ^");
    let tokens = lexer.tokenize().unwrap();

    let expected_operators = vec![
      TokenKind::Plus,
      TokenKind::Minus,
      TokenKind::Star,
      TokenKind::Slash,
      TokenKind::Percent,
      TokenKind::Equal,
      TokenKind::NotEqual,
      TokenKind::Less,
      TokenKind::LessEqual,
      TokenKind::Greater,
      TokenKind::GreaterEqual,
      TokenKind::And,
      TokenKind::Or,
      TokenKind::Not,
      TokenKind::BitAnd,
      TokenKind::BitOr,
      TokenKind::BitXor,
    ];

    assert_eq!(tokens.len(), expected_operators.len());
    for (i, expected) in expected_operators.iter().enumerate() {
      assert_eq!(tokens[i].kind, *expected);
    }
  }

  #[test]
  fn test_lexer_literals() {
    let mut lexer = Lexer::from_str(r#""hello" 'a' 42 3.14 true false"#);
    let tokens = lexer.tokenize().unwrap();

    assert_eq!(tokens.len(), 6);
    assert_eq!(tokens[0].kind, TokenKind::String);
    assert_eq!(tokens[1].kind, TokenKind::Char);
    assert_eq!(tokens[2].kind, TokenKind::Integer);
    assert_eq!(tokens[3].kind, TokenKind::Float);
    assert_eq!(tokens[4].kind, TokenKind::Boolean);
    assert_eq!(tokens[5].kind, TokenKind::Boolean);
  }

  #[test]
  fn test_lexer_identifiers() {
    let mut lexer = Lexer::from_str("x y_123 _private CONSTANT");
    let tokens = lexer.tokenize().unwrap();

    assert_eq!(tokens.len(), 4);
    for token in tokens {
      assert_eq!(token.kind, TokenKind::Identifier);
    }
  }

  #[test]
  fn test_lexer_comments() {
    let mut lexer = Lexer::from_str("// line comment\n/* block comment */\n/// doc comment");
    let tokens = lexer.tokenize_all().unwrap();

    // Should have comment tokens
    let comment_tokens: Vec<_> = tokens
      .iter()
      .filter(|t| {
        matches!(
          t.kind,
          TokenKind::LineComment | TokenKind::BlockComment | TokenKind::DocComment
        )
      })
      .collect();

    assert!(!comment_tokens.is_empty());
  }

  #[test]
  fn test_lexer_reset() {
    let mut lexer = Lexer::from_str("let x = 42");
    let tokens1 = lexer.tokenize().unwrap();
    assert_eq!(tokens1.len(), 4);

    lexer.reset();
    let tokens2 = lexer.tokenize().unwrap();
    assert_eq!(tokens2.len(), 4);
    assert_eq!(tokens1, tokens2);
  }

  #[test]
  fn test_lexer_peek() {
    let mut lexer = Lexer::from_str("let x = 42");

    // Peek should not consume tokens
    let peeked = lexer.peek();
    assert!(peeked.is_some());
    assert_eq!(peeked.unwrap(), TokenKind::Let);

    // First token should still be available
    let tokens = lexer.tokenize().unwrap();
    assert_eq!(tokens[0].kind, TokenKind::Let);
  }

  #[test]
  fn test_lexer_is_eof() {
    let mut lexer = Lexer::from_str("let x");
    assert!(!lexer.is_eof());

    let _tokens = lexer.tokenize().unwrap();
    assert!(lexer.is_eof());
  }

  #[test]
  fn test_lexer_remaining() {
    let mut lexer = Lexer::from_str("let x = 42");
    assert_eq!(lexer.remaining(), "let x = 42");

    let _tokens = lexer.tokenize().unwrap();
    assert_eq!(lexer.remaining(), "");
  }
}
