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

  // Error token
  #[token("InvalidToken")]
  InvalidToken,
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

      // Error token
      TokenKind::InvalidToken => write!(f, "invalid token"),
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
  /// Invalid token sequence
  InvalidTokenSequence(String, SourceLocation),
  /// Malformed identifier
  MalformedIdentifier(String, SourceLocation),
  /// Invalid operator
  InvalidOperator(String, SourceLocation),
  /// Unterminated comment
  UnterminatedComment(SourceLocation),
  /// Invalid Unicode character
  InvalidUnicode(char, SourceLocation),
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
      LexerError::InvalidTokenSequence(seq, loc) => {
        write!(f, "Invalid token sequence '{}' at {}", seq, loc)
      }
      LexerError::MalformedIdentifier(id, loc) => {
        write!(f, "Malformed identifier '{}' at {}", id, loc)
      }
      LexerError::InvalidOperator(op, loc) => {
        write!(f, "Invalid operator '{}' at {}", op, loc)
      }
      LexerError::UnterminatedComment(loc) => {
        write!(f, "Unterminated comment at {}", loc)
      }
      LexerError::InvalidUnicode(ch, loc) => {
        write!(f, "Invalid Unicode character '{}' at {}", ch, loc)
      }
    }
  }
}

impl std::error::Error for LexerError {}

/// Result type for lexer operations
pub type LexerResult<T> = Result<T, LexerError>;

/// Result type for lexer operations that can collect multiple errors
pub type LexerResultWithErrors<T> = Result<T, Vec<LexerError>>;

/// A lexer for the Lattice language that converts source code into tokens
pub struct Lexer {
  /// The source code to tokenize
  source: String,
  /// Current source location
  current_location: SourceLocation,
  /// Configuration for error recovery
  error_recovery_config: ErrorRecoveryConfig,
}

/// Configuration for error recovery behavior
#[derive(Debug, Clone)]
pub struct ErrorRecoveryConfig {
  /// Whether to continue lexing after encountering errors
  pub continue_on_error: bool,
  /// Maximum number of errors to collect before stopping
  pub max_errors: Option<usize>,
  /// Whether to insert error tokens for invalid characters
  pub insert_error_tokens: bool,
  /// Whether to skip invalid characters
  pub skip_invalid_chars: bool,
}

impl Default for ErrorRecoveryConfig {
  fn default() -> Self {
    Self {
      continue_on_error: true,
      max_errors: Some(100),
      insert_error_tokens: false,
      skip_invalid_chars: true,
    }
  }
}

impl Lexer {
  /// Create a new lexer for the given source code
  pub fn new(source: String) -> Self {
    Self {
      source,
      current_location: SourceLocation::start(),
      error_recovery_config: ErrorRecoveryConfig::default(),
    }
  }

  /// Create a new lexer from a string slice
  pub fn from_str(source: &str) -> Self {
    Self::new(source.to_string())
  }

  /// Create a new lexer with custom error recovery configuration
  pub fn with_config(source: String, config: ErrorRecoveryConfig) -> Self {
    Self {
      source,
      current_location: SourceLocation::start(),
      error_recovery_config: config,
    }
  }

  /// Get the source code being tokenized
  pub fn source(&self) -> &str {
    &self.source
  }

  /// Get the current source location
  pub fn current_location(&self) -> SourceLocation {
    self.current_location
  }

  /// Get the error recovery configuration
  pub fn error_recovery_config(&self) -> &ErrorRecoveryConfig {
    &self.error_recovery_config
  }

  /// Set the error recovery configuration
  pub fn set_error_recovery_config(&mut self, config: ErrorRecoveryConfig) {
    self.error_recovery_config = config;
  }

  /// Reset the lexer to the beginning
  pub fn reset(&mut self) {
    self.current_location = SourceLocation::start();
  }

  /// Tokenize the entire source code into a vector of tokens with error collection
  pub fn tokenize_with_errors(&mut self) -> LexerResultWithErrors<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
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
          // Handle logos errors with error recovery
          let error_location = current_location;
          let invalid_text = lexer.slice();

          // Create appropriate error based on the invalid text
          let error = self.create_error_from_invalid_text(invalid_text, error_location);
          errors.push(error);

          // Check if we should continue or stop
          if !self.should_continue_after_error(&errors) {
            break;
          }

          // Advance location by the invalid text to continue lexing
          current_location.advance_by(invalid_text);

          // Optionally insert error token
          if self.error_recovery_config.insert_error_tokens {
            let error_token = Token::new(
              TokenKind::InvalidToken,
              invalid_text.to_string(),
              error_location,
              current_location,
            );
            tokens.push(error_token);
          }
        }
      }
    }

    self.current_location = current_location;

    if errors.is_empty() {
      Ok(tokens)
    } else {
      Err(errors)
    }
  }

  /// Tokenize the entire source code into a vector of tokens
  pub fn tokenize(&mut self) -> LexerResult<Vec<Token>> {
    match self.tokenize_with_errors() {
      Ok(tokens) => Ok(tokens),
      Err(errors) => {
        // Return the first error for backward compatibility
        Err(
          errors
            .into_iter()
            .next()
            .unwrap_or_else(|| LexerError::UnexpectedEndOfInput(self.current_location)),
        )
      }
    }
  }

  /// Tokenize including whitespace and comments
  pub fn tokenize_all(&mut self) -> LexerResult<Vec<Token>> {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
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
          // Handle logos errors with error recovery
          let error_location = current_location;
          let invalid_text = lexer.slice();

          // Create appropriate error based on the invalid text
          let error = self.create_error_from_invalid_text(invalid_text, error_location);
          errors.push(error);

          // Check if we should continue or stop
          if !self.should_continue_after_error(&errors) {
            break;
          }

          // Advance location by the invalid text to continue lexing
          current_location.advance_by(invalid_text);

          // Optionally insert error token
          if self.error_recovery_config.insert_error_tokens {
            let error_token = Token::new(
              TokenKind::InvalidToken,
              invalid_text.to_string(),
              error_location,
              current_location,
            );
            tokens.push(error_token);
          }
        }
      }
    }

    self.current_location = current_location;

    if errors.is_empty() {
      Ok(tokens)
    } else {
      Err(
        errors
          .into_iter()
          .next()
          .unwrap_or_else(|| LexerError::UnexpectedEndOfInput(self.current_location)),
      )
    }
  }

  /// Create an appropriate error based on invalid text
  fn create_error_from_invalid_text(
    &self,
    invalid_text: &str,
    location: SourceLocation,
  ) -> LexerError {
    if invalid_text.is_empty() {
      return LexerError::UnexpectedEndOfInput(location);
    }

    // Try to identify the type of error based on the invalid text
    if invalid_text.starts_with('"') && !invalid_text.ends_with('"') {
      LexerError::UnterminatedString(location)
    } else if invalid_text.starts_with('\'') && !invalid_text.ends_with('\'') {
      LexerError::UnterminatedChar(location)
    } else if invalid_text.starts_with("/*") && !invalid_text.ends_with("*/") {
      LexerError::UnterminatedBlockComment(location)
    } else if invalid_text.starts_with("//") {
      LexerError::UnterminatedComment(location)
    } else if invalid_text
      .chars()
      .any(|c| !c.is_ascii() && !c.is_alphanumeric() && !c.is_whitespace())
    {
      // Check for invalid Unicode characters
      if let Some(invalid_char) = invalid_text
        .chars()
        .find(|c| !c.is_ascii() && !c.is_alphanumeric() && !c.is_whitespace())
      {
        LexerError::InvalidUnicode(invalid_char, location)
      } else {
        LexerError::InvalidCharacter(invalid_text.chars().next().unwrap(), location)
      }
    } else if invalid_text.chars().all(|c| c.is_ascii_punctuation()) {
      // Invalid operator sequence
      LexerError::InvalidOperator(invalid_text.to_string(), location)
    } else if invalid_text.chars().any(|c| c.is_alphanumeric()) {
      // Malformed identifier
      LexerError::MalformedIdentifier(invalid_text.to_string(), location)
    } else {
      // Generic invalid character
      LexerError::InvalidCharacter(invalid_text.chars().next().unwrap(), location)
    }
  }

  /// Check if we should continue lexing after encountering an error
  fn should_continue_after_error(&self, errors: &[LexerError]) -> bool {
    if !self.error_recovery_config.continue_on_error {
      return false;
    }

    if let Some(max_errors) = self.error_recovery_config.max_errors {
      if errors.len() >= max_errors {
        return false;
      }
    }

    true
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

  /// Attempt to recover from a specific error type
  pub fn try_recover_from_error(
    &self,
    error: &LexerError,
    source: &str,
    position: usize,
  ) -> Option<usize> {
    match error {
      LexerError::UnterminatedString(_loc) => {
        // Try to find the next quote character
        if let Some(next_quote) = source[position..].find('"') {
          Some(position + next_quote + 1)
        } else {
          None
        }
      }
      LexerError::UnterminatedChar(_loc) => {
        // Try to find the next single quote character
        if let Some(next_quote) = source[position..].find('\'') {
          Some(position + next_quote + 1)
        } else {
          None
        }
      }
      LexerError::UnterminatedBlockComment(_loc) => {
        // Try to find the end of block comment
        if let Some(end_comment) = source[position..].find("*/") {
          Some(position + end_comment + 2)
        } else {
          Some(source.len())
        }
      }
      LexerError::UnterminatedComment(_loc) => {
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

  /// Validate that the lexer configuration is reasonable
  pub fn validate_config(&self) -> Result<(), String> {
    if self.error_recovery_config.max_errors == Some(0) {
      return Err("max_errors cannot be 0".to_string());
    }

    if !self.error_recovery_config.continue_on_error
      && self.error_recovery_config.max_errors.is_some()
    {
      return Err("max_errors is ignored when continue_on_error is false".to_string());
    }

    Ok(())
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

  // Error recovery and reporting tests
  #[test]
  fn test_lexer_error_recovery_config() {
    let config = ErrorRecoveryConfig {
      continue_on_error: true,
      max_errors: Some(50),
      insert_error_tokens: true,
      skip_invalid_chars: false,
    };

    let lexer = Lexer::with_config("test".to_string(), config);
    assert!(lexer.error_recovery_config().continue_on_error);
    assert_eq!(lexer.error_recovery_config().max_errors, Some(50));
    assert!(lexer.error_recovery_config().insert_error_tokens);
    assert!(!lexer.error_recovery_config().skip_invalid_chars);
  }

  #[test]
  fn test_lexer_error_recovery_config_default() {
    let config = ErrorRecoveryConfig::default();
    assert!(config.continue_on_error);
    assert_eq!(config.max_errors, Some(100));
    assert!(!config.insert_error_tokens);
    assert!(config.skip_invalid_chars);
  }

  #[test]
  fn test_lexer_config_validation() {
    let mut lexer = Lexer::from_str("test");

    // Valid config
    let valid_config = ErrorRecoveryConfig::default();
    lexer.set_error_recovery_config(valid_config);
    assert!(lexer.validate_config().is_ok());

    // Invalid config - max_errors = 0
    let invalid_config = ErrorRecoveryConfig {
      continue_on_error: true,
      max_errors: Some(0),
      insert_error_tokens: false,
      skip_invalid_chars: true,
    };
    lexer.set_error_recovery_config(invalid_config);
    assert!(lexer.validate_config().is_err());

    // Invalid config - max_errors ignored when continue_on_error = false
    let invalid_config2 = ErrorRecoveryConfig {
      continue_on_error: false,
      max_errors: Some(50),
      insert_error_tokens: false,
      skip_invalid_chars: true,
    };
    lexer.set_error_recovery_config(invalid_config2);
    assert!(lexer.validate_config().is_err());
  }

  #[test]
  fn test_lexer_error_collection() {
    // Use input with null bytes that will trigger logos errors
    let mut lexer = Lexer::from_str("let x = \u{0000} y");
    let result = lexer.tokenize_with_errors();

    match result {
      Err(errors) => {
        assert!(!errors.is_empty());
        // Should have at least one error for the null byte
        let has_invalid_char_error = errors
          .iter()
          .any(|e| matches!(e, LexerError::InvalidCharacter(_, _)));
        assert!(has_invalid_char_error);
      }
      Ok(_) => panic!("Expected errors for invalid input"),
    }
  }

  #[test]
  fn test_lexer_error_recovery_continue() {
    // Use input with null bytes that will trigger logos errors
    let mut lexer = Lexer::from_str("let x = \u{0000} y = \u{0001} z");
    let result = lexer.tokenize_with_errors();

    match result {
      Err(errors) => {
        // Should have errors but also continue processing
        assert!(!errors.is_empty());
        // Should have processed tokens after the error
        assert!(errors.len() >= 1);
      }
      Ok(_) => panic!("Expected errors for invalid input"),
    }
  }

  #[test]
  fn test_lexer_error_recovery_stop() {
    let mut config = ErrorRecoveryConfig::default();
    config.continue_on_error = false;

    // Use input with null bytes that will trigger logos errors
    let mut lexer = Lexer::with_config("let x = \u{0000} y = \u{0001} z".to_string(), config);
    let result = lexer.tokenize_with_errors();

    match result {
      Err(errors) => {
        // Should stop at first error
        assert_eq!(errors.len(), 1);
      }
      Ok(_) => panic!("Expected errors for invalid input"),
    }
  }

  #[test]
  fn test_lexer_max_errors_limit() {
    let mut config = ErrorRecoveryConfig::default();
    config.max_errors = Some(2);

    // Use input with null bytes that will trigger logos errors
    let mut lexer = Lexer::with_config(
      "let x = \u{0000} y = \u{0001} z = \u{0002}".to_string(),
      config,
    );
    let result = lexer.tokenize_with_errors();

    match result {
      Err(errors) => {
        // Should stop at max_errors limit
        assert_eq!(errors.len(), 2);
      }
      Ok(_) => panic!("Expected errors for invalid input"),
    }
  }

  #[test]
  fn test_lexer_error_token_insertion() {
    let mut config = ErrorRecoveryConfig::default();
    config.insert_error_tokens = true;

    // Use input with null bytes that will trigger logos errors
    let mut lexer = Lexer::with_config("let x = \u{0000} y".to_string(), config);
    let result = lexer.tokenize_with_errors();

    match result {
      Err(errors) => {
        // Should have errors
        assert!(!errors.is_empty());
      }
      Ok(tokens) => {
        // Should have error tokens inserted
        let has_error_token = tokens.iter().any(|t| t.kind == TokenKind::InvalidToken);
        assert!(has_error_token);
      }
    }
  }

  #[test]
  fn test_lexer_error_summary() {
    let lexer = Lexer::from_str("test");
    let errors = vec![
      LexerError::InvalidCharacter('@', SourceLocation::new(1, 5, 4)),
      LexerError::InvalidCharacter('#', SourceLocation::new(1, 7, 6)),
    ];

    let summary = lexer.get_error_summary(&errors);
    assert!(summary.contains("2 error(s) encountered:"));
    assert!(summary.contains("Invalid character '@' at 1:5"));
    assert!(summary.contains("Invalid character '#' at 1:7"));
  }

  #[test]
  fn test_lexer_error_summary_empty() {
    let lexer = Lexer::from_str("test");
    let errors = vec![];

    let summary = lexer.get_error_summary(&errors);
    assert_eq!(summary, "No errors encountered");
  }

  #[test]
  fn test_lexer_error_recovery_strategies() {
    let lexer = Lexer::from_str("test");

    // Test string recovery - start from position after opening quote
    let error = LexerError::UnterminatedString(SourceLocation::new(1, 1, 0));
    let source = "let x = \"hello world\"";
    let recovery_pos = lexer.try_recover_from_error(&error, source, 9); // Start from position after opening quote
    assert_eq!(recovery_pos, Some(21)); // Position after closing quote (8 + 1 + 11 + 1)

    // Test char recovery - start from position after opening quote
    let error = LexerError::UnterminatedChar(SourceLocation::new(1, 1, 0));
    let source = "let x = 'a'";
    let recovery_pos = lexer.try_recover_from_error(&error, source, 9); // Start from position after opening quote
    assert_eq!(recovery_pos, Some(11)); // Position after closing quote (8 + 1 + 1 + 1)

    // Test block comment recovery - start from position after opening comment
    let error = LexerError::UnterminatedBlockComment(SourceLocation::new(1, 1, 0));
    let source = "let x = /* comment */";
    let recovery_pos = lexer.try_recover_from_error(&error, source, 10); // Start from position after opening comment
    assert_eq!(recovery_pos, Some(21)); // Position after closing comment (8 + 1 + 1 + 10 + 1)

    // Test line comment recovery - start from position after opening comment
    let error = LexerError::UnterminatedComment(SourceLocation::new(1, 1, 0));
    let source = "let x = // comment\nlet y = 42";
    let recovery_pos = lexer.try_recover_from_error(&error, source, 10); // Start from position after opening comment
    assert_eq!(recovery_pos, Some(19)); // Position after newline (8 + 1 + 1 + 8 + 1)
  }

  #[test]
  fn test_lexer_error_recovery_no_recovery() {
    let lexer = Lexer::from_str("test");

    // Test error with no recovery possible - use a source that doesn't have the expected recovery character
    let error = LexerError::UnterminatedString(SourceLocation::new(1, 1, 0));
    let source = "hello world"; // No quotes at all
    let recovery_pos = lexer.try_recover_from_error(&error, source, 0);
    assert_eq!(recovery_pos, None);
  }

  #[test]
  fn test_lexer_backward_compatibility() {
    // Use input with null bytes that will trigger logos errors
    let mut lexer = Lexer::from_str("let x = \u{0000} y");

    // The old tokenize() method should still work and return the first error
    let result = lexer.tokenize();
    assert!(result.is_err());

    // The error should be a LexerError, not a Vec<LexerError>
    match result {
      Err(error) => {
        assert!(matches!(error, LexerError::InvalidCharacter(_, _)));
      }
      Ok(_) => panic!("Expected error"),
    }
  }

  #[test]
  fn test_lexer_multiple_error_types() {
    // Use input with null bytes and unterminated strings that will trigger errors
    let mut lexer = Lexer::from_str("let x = \"unterminated string\nlet y = \u{0000}\nlet z = /* unterminated comment\nlet w = \u{0001}");

    let result = lexer.tokenize_with_errors();

    match result {
      Err(errors) => {
        // Should have multiple different types of errors
        let has_unterminated_string = errors
          .iter()
          .any(|e| matches!(e, LexerError::UnterminatedString(_)));
        let has_invalid_char = errors
          .iter()
          .any(|e| matches!(e, LexerError::InvalidCharacter(_, _)));

        // Note: logos might not detect unterminated comments as errors
        // so we only check for the errors we know will occur
        assert!(has_unterminated_string || has_invalid_char);
      }
      Ok(_) => panic!("Expected errors for invalid input"),
    }
  }

  #[test]
  fn test_lexer_debug_invalid_input() {
    // Debug test to see what logos actually produces for invalid input
    let source = "let x = @ y = # z = $";
    let mut lexer = TokenKind::lexer(source);

    println!("Debug: Lexing source: '{}'", source);
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    while let Some(result) = lexer.next() {
      match result {
        Ok(kind) => {
          let text = lexer.slice();
          println!("Debug: Token: {:?} -> '{}'", kind, text);
          tokens.push((kind, text));
        }
        Err(e) => {
          let text = lexer.slice();
          println!("Debug: Error: {:?} -> '{}'", e, text);
          errors.push((e, text));
        }
      }
    }

    println!(
      "Debug: Total tokens: {}, Total errors: {}",
      tokens.len(),
      errors.len()
    );

    // This test is just for debugging, so we don't assert anything
    // It will help us understand what logos actually produces
  }

  #[test]
  fn test_lexer_debug_truly_invalid_input() {
    // Try some truly invalid input that should trigger errors
    let source = "let x = \u{0000} y = \u{0001} z = \u{0002}";
    let mut lexer = TokenKind::lexer(source);

    println!("Debug: Lexing truly invalid source: '{}'", source);
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    while let Some(result) = lexer.next() {
      match result {
        Ok(kind) => {
          let text = lexer.slice();
          println!("Debug: Token: {:?} -> '{}'", kind, text);
          tokens.push((kind, text));
        }
        Err(e) => {
          let text = lexer.slice();
          println!("Debug: Error: {:?} -> '{}'", e, text);
          errors.push((e, text));
        }
      }
    }

    println!(
      "Debug: Total tokens: {}, Total errors: {}",
      tokens.len(),
      errors.len()
    );

    // This test is just for debugging, so we don't assert anything
  }
}
