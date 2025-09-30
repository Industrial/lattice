//! Abstract Syntax Tree (AST) definitions for the Lattice language.
//!
//! This module provides the data structures for representing the parsed structure
//! of Lattice programs, including expressions, statements, declarations, patterns,
//! and type expressions.

use crate::lexer::{SourceLocation, Token};
use crate::types::types::Type;
use std::fmt;

/// Represents a span in the source code (start to end location)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
  /// Start location
  pub start: SourceLocation,
  /// End location
  pub end: SourceLocation,
}

impl Span {
  /// Create a new span from start to end locations
  pub fn new(start: SourceLocation, end: SourceLocation) -> Self {
    Self { start, end }
  }

  /// Create a span from a single token
  pub fn from_token(token: &Token) -> Self {
    Self {
      start: token.start,
      end: token.end,
    }
  }

  /// Create a span that encompasses multiple spans
  pub fn from_spans(spans: &[Span]) -> Option<Self> {
    if spans.is_empty() {
      return None;
    }

    let start = spans.iter().map(|s| s.start).min().unwrap();
    let end = spans.iter().map(|s| s.end).max().unwrap();
    Some(Self { start, end })
  }

  /// Extend the span to include another span
  pub fn extend(&mut self, other: &Span) {
    if other.start < self.start {
      self.start = other.start;
    }
    if other.end > self.end {
      self.end = other.end;
    }
  }
}

impl fmt::Display for Span {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}:{}", self.start, self.end)
  }
}

/// Represents an identifier in the AST
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
  /// The name of the identifier
  pub name: String,
  /// The source location span
  pub span: Span,
}

impl Identifier {
  /// Create a new identifier
  pub fn new(name: String, span: Span) -> Self {
    Self { name, span }
  }

  /// Create an identifier from a token
  pub fn from_token(token: &Token) -> Self {
    Self {
      name: token.text.clone(),
      span: Span::from_token(token),
    }
  }
}

impl fmt::Display for Identifier {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.name)
  }
}

/// Represents a literal value in the AST
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
  /// Integer literal
  Integer { value: i64, span: Span },
  /// Floating-point literal
  Float { value: f64, span: Span },
  /// String literal
  String { value: String, span: Span },
  /// Character literal
  Char { value: char, span: Span },
  /// Boolean literal
  Boolean { value: bool, span: Span },
  /// Unit literal (empty tuple)
  Unit { span: Span },
}

impl Literal {
  /// Get the span of the literal
  pub fn span(&self) -> Span {
    match self {
      Literal::Integer { span, .. } => *span,
      Literal::Float { span, .. } => *span,
      Literal::String { span, .. } => *span,
      Literal::Char { span, .. } => *span,
      Literal::Boolean { span, .. } => *span,
      Literal::Unit { span } => *span,
    }
  }
}

impl fmt::Display for Literal {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Literal::Integer { value, .. } => write!(f, "{}", value),
      Literal::Float { value, .. } => write!(f, "{}", value),
      Literal::String { value, .. } => write!(f, "\"{}\"", value),
      Literal::Char { value, .. } => write!(f, "'{}'", value),
      Literal::Boolean { value, .. } => write!(f, "{}", value),
      Literal::Unit { .. } => write!(f, "()"),
    }
  }
}

/// Represents a pattern in pattern matching
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
  /// Variable pattern (e.g., `x`)
  Variable { identifier: Identifier, span: Span },
  /// Wildcard pattern (e.g., `_`)
  Wildcard { span: Span },
  /// Literal pattern (e.g., `42`, `"hello"`)
  Literal { literal: Literal, span: Span },
  /// Constructor pattern (e.g., `Some(x)`, `Cons(head, tail)`)
  Constructor {
    constructor: Identifier,
    arguments: Vec<Pattern>,
    span: Span,
  },
  /// Tuple pattern (e.g., `(x, y)`)
  Tuple { elements: Vec<Pattern>, span: Span },
  /// List pattern (e.g., `[x, y, z]`)
  List { elements: Vec<Pattern>, span: Span },
  /// Or-pattern (e.g., `x | y`)
  Or {
    left: Box<Pattern>,
    right: Box<Pattern>,
    span: Span,
  },
  /// As-pattern (e.g., `x as y`)
  As {
    pattern: Box<Pattern>,
    identifier: Identifier,
    span: Span,
  },
}

impl Pattern {
  /// Get the span of the pattern
  pub fn span(&self) -> Span {
    match self {
      Pattern::Variable { span, .. } => *span,
      Pattern::Wildcard { span } => *span,
      Pattern::Literal { span, .. } => *span,
      Pattern::Constructor { span, .. } => *span,
      Pattern::Tuple { span, .. } => *span,
      Pattern::List { span, .. } => *span,
      Pattern::Or { span, .. } => *span,
      Pattern::As { span, .. } => *span,
    }
  }
}

impl fmt::Display for Pattern {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Pattern::Variable { identifier, .. } => write!(f, "{}", identifier),
      Pattern::Wildcard { .. } => write!(f, "_"),
      Pattern::Literal { literal, .. } => write!(f, "{}", literal),
      Pattern::Constructor {
        constructor,
        arguments,
        ..
      } => {
        if arguments.is_empty() {
          write!(f, "{}", constructor)
        } else {
          write!(
            f,
            "{}({})",
            constructor,
            arguments
              .iter()
              .map(|p| p.to_string())
              .collect::<Vec<_>>()
              .join(", ")
          )
        }
      }
      Pattern::Tuple { elements, .. } => {
        write!(
          f,
          "({})",
          elements
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(", ")
        )
      }
      Pattern::List { elements, .. } => {
        write!(
          f,
          "[{}]",
          elements
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(", ")
        )
      }
      Pattern::Or { left, right, .. } => {
        write!(f, "{} | {}", left, right)
      }
      Pattern::As {
        pattern,
        identifier,
        ..
      } => {
        write!(f, "{} as {}", pattern, identifier)
      }
    }
  }
}

/// Represents a type expression in the AST
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
  /// Type variable (e.g., `'a`)
  Variable { name: String, span: Span },
  /// Type constructor (e.g., `Int`, `String`)
  Constructor { name: Identifier, span: Span },
  /// Function type (e.g., `Int -> String`)
  Function {
    parameter: Box<TypeExpr>,
    return_type: Box<TypeExpr>,
    span: Span,
  },
  /// Tuple type (e.g., `(Int, String)`)
  Tuple { elements: Vec<TypeExpr>, span: Span },
  /// List type (e.g., `[Int]`)
  List {
    element_type: Box<TypeExpr>,
    span: Span,
  },
  /// Generic type application (e.g., `Option<Int>`)
  Generic {
    constructor: Identifier,
    arguments: Vec<TypeExpr>,
    span: Span,
  },
  /// Effect type (e.g., `Int !> String`)
  Effect {
    input_type: Box<TypeExpr>,
    output_type: Box<TypeExpr>,
    span: Span,
  },
}

impl TypeExpr {
  /// Get the span of the type expression
  pub fn span(&self) -> Span {
    match self {
      TypeExpr::Variable { span, .. } => *span,
      TypeExpr::Constructor { span, .. } => *span,
      TypeExpr::Function { span, .. } => *span,
      TypeExpr::Tuple { span, .. } => *span,
      TypeExpr::List { span, .. } => *span,
      TypeExpr::Generic { span, .. } => *span,
      TypeExpr::Effect { span, .. } => *span,
    }
  }
}

impl fmt::Display for TypeExpr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TypeExpr::Variable { name, .. } => write!(f, "'{}", name),
      TypeExpr::Constructor { name, .. } => write!(f, "{}", name),
      TypeExpr::Function {
        parameter,
        return_type,
        ..
      } => {
        write!(f, "{} -> {}", parameter, return_type)
      }
      TypeExpr::Tuple { elements, .. } => {
        write!(
          f,
          "({})",
          elements
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(", ")
        )
      }
      TypeExpr::List { element_type, .. } => {
        write!(f, "[{}]", element_type)
      }
      TypeExpr::Generic {
        constructor,
        arguments,
        ..
      } => {
        write!(
          f,
          "{}<{}>",
          constructor,
          arguments
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(", ")
        )
      }
      TypeExpr::Effect {
        input_type,
        output_type,
        ..
      } => {
        write!(f, "{} !> {}", input_type, output_type)
      }
    }
  }
}

/// Represents an expression in the AST
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
  /// Literal expression
  Literal {
    literal: Literal,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Variable reference
  Variable {
    identifier: Identifier,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Function application (e.g., `f(x, y)`)
  Application {
    function: Box<Expression>,
    arguments: Vec<Expression>,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Binary operation (e.g., `x + y`)
  BinaryOp {
    left: Box<Expression>,
    operator: BinaryOperator,
    right: Box<Expression>,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Unary operation (e.g., `!x`)
  UnaryOp {
    operator: UnaryOperator,
    operand: Box<Expression>,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Let expression (e.g., `let x = 42 in x + 1`)
  Let {
    bindings: Vec<Binding>,
    body: Box<Expression>,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// If expression (e.g., `if x > 0 then x else -x`)
  If {
    condition: Box<Expression>,
    then_branch: Box<Expression>,
    else_branch: Box<Expression>,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Match expression (e.g., `match x with | Some(y) => y | None => 0`)
  Match {
    scrutinee: Box<Expression>,
    arms: Vec<MatchArm>,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Lambda expression (e.g., `\x -> x + 1`)
  Lambda {
    parameters: Vec<Pattern>,
    body: Box<Expression>,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Tuple expression (e.g., `(x, y)`)
  Tuple {
    elements: Vec<Expression>,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// List expression (e.g., `[1, 2, 3]`)
  List {
    elements: Vec<Expression>,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Effect operation (e.g., `do Get`)
  EffectOp {
    operation: Identifier,
    arguments: Vec<Expression>,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Effect handler (e.g., `handle e with Get => resume 42`)
  Handler {
    expression: Box<Expression>,
    cases: Vec<HandlerCase>,
    span: Span,
    type_annotation: Option<Type>,
  },
}

impl Expression {
  /// Get the span of the expression
  pub fn span(&self) -> Span {
    match self {
      Expression::Literal { span, .. } => *span,
      Expression::Variable { span, .. } => *span,
      Expression::Application { span, .. } => *span,
      Expression::BinaryOp { span, .. } => *span,
      Expression::UnaryOp { span, .. } => *span,
      Expression::Let { span, .. } => *span,
      Expression::If { span, .. } => *span,
      Expression::Match { span, .. } => *span,
      Expression::Lambda { span, .. } => *span,
      Expression::Tuple { span, .. } => *span,
      Expression::List { span, .. } => *span,
      Expression::EffectOp { span, .. } => *span,
      Expression::Handler { span, .. } => *span,
      _ => todo!(),
    }
  }

  /// Get the type annotation of the expression
  pub fn type_annotation(&self) -> Option<&Type> {
    match self {
      Expression::Literal {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Expression::Variable {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Expression::Application {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Expression::BinaryOp {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Expression::UnaryOp {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Expression::Let {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Expression::If {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Expression::Match {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Expression::Lambda {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Expression::Tuple {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Expression::List {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Expression::EffectOp {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Expression::Handler {
        type_annotation, ..
      } => type_annotation.as_ref(),
    }
  }

  /// Set the type annotation of the expression
  pub fn set_type_annotation(&mut self, annotation: Option<Type>) {
    match self {
      Expression::Literal {
        type_annotation, ..
      } => *type_annotation = annotation,
      Expression::Variable {
        type_annotation, ..
      } => *type_annotation = annotation,
      Expression::Application {
        type_annotation, ..
      } => *type_annotation = annotation,
      Expression::BinaryOp {
        type_annotation, ..
      } => *type_annotation = annotation,
      Expression::UnaryOp {
        type_annotation, ..
      } => *type_annotation = annotation,
      Expression::Let {
        type_annotation, ..
      } => *type_annotation = annotation,
      Expression::If {
        type_annotation, ..
      } => *type_annotation = annotation,
      Expression::Match {
        type_annotation, ..
      } => *type_annotation = annotation,
      Expression::Lambda {
        type_annotation, ..
      } => *type_annotation = annotation,
      Expression::Tuple {
        type_annotation, ..
      } => *type_annotation = annotation,
      Expression::List {
        type_annotation, ..
      } => *type_annotation = annotation,
      Expression::EffectOp {
        type_annotation, ..
      } => *type_annotation = annotation,
      Expression::Handler {
        type_annotation, ..
      } => *type_annotation = annotation,
    }
  }
}

impl fmt::Display for Expression {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Expression::Literal {
        type_annotation: None,
        literal,
        ..
      } => write!(f, "{}", literal),
      Expression::Variable {
        type_annotation: None,
        identifier,
        ..
      } => write!(f, "{}", identifier),
      Expression::Application {
        type_annotation: None,
        function,
        arguments,
        ..
      } => {
        if arguments.is_empty() {
          write!(f, "{}", function)
        } else {
          write!(
            f,
            "{}({})",
            function,
            arguments
              .iter()
              .map(|e| e.to_string())
              .collect::<Vec<_>>()
              .join(", ")
          )
        }
      }
      Expression::BinaryOp {
        type_annotation: None,
        left,
        operator,
        right,
        ..
      } => {
        write!(f, "{} {} {}", left, operator, right)
      }
      Expression::UnaryOp {
        type_annotation: None,
        operator,
        operand,
        ..
      } => {
        write!(f, "{}{}", operator, operand)
      }
      Expression::Let {
        type_annotation: None,
        bindings,
        body,
        ..
      } => {
        write!(
          f,
          "let {} in {}",
          bindings
            .iter()
            .map(|b| b.to_string())
            .collect::<Vec<_>>()
            .join("; "),
          body
        )
      }
      Expression::If {
        type_annotation: None,
        condition,
        then_branch,
        else_branch,
        ..
      } => {
        write!(
          f,
          "if {} then {} else {}",
          condition, then_branch, else_branch
        )
      }
      Expression::Match {
        type_annotation: None,
        scrutinee,
        arms,
        ..
      } => {
        write!(
          f,
          "match {} with {}",
          scrutinee,
          arms
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<_>>()
            .join(" | ")
        )
      }
      Expression::Lambda {
        type_annotation: None,
        parameters,
        body,
        ..
      } => {
        write!(
          f,
          "\\{} -> {}",
          parameters
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(" "),
          body
        )
      }
      Expression::Tuple {
        type_annotation: None,
        elements,
        ..
      } => {
        write!(
          f,
          "({})",
          elements
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(", ")
        )
      }
      Expression::List {
        type_annotation: None,
        elements,
        ..
      } => {
        write!(
          f,
          "[{}]",
          elements
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join(", ")
        )
      }
      Expression::EffectOp {
        type_annotation: None,
        operation,
        arguments,
        ..
      } => {
        if arguments.is_empty() {
          write!(f, "do {}", operation)
        } else {
          write!(
            f,
            "do {}({})",
            operation,
            arguments
              .iter()
              .map(|e| e.to_string())
              .collect::<Vec<_>>()
              .join(", ")
          )
        }
      }
      Expression::Handler {
        type_annotation: None,
        expression,
        cases,
        ..
      } => {
        write!(
          f,
          "handle {} with {}",
          expression,
          cases
            .iter()
            .map(|c| c.to_string())
            .collect::<Vec<_>>()
            .join(" | ")
        )
      }
      _ => todo!(),
    }
  }
}

/// Represents a binary operator
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
  /// Addition
  Add,
  /// Subtraction
  Sub,
  /// Multiplication
  Mul,
  /// Division
  Div,
  /// Modulo
  Mod,
  /// Exponentiation
  Pow,
  /// Equality
  Eq,
  /// Inequality
  Ne,
  /// Less than
  Lt,
  /// Less than or equal
  Le,
  /// Greater than
  Gt,
  /// Greater than or equal
  Ge,
  /// Logical AND
  And,
  /// Logical OR
  Or,
}

impl fmt::Display for BinaryOperator {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      BinaryOperator::Add => write!(f, "+"),
      BinaryOperator::Sub => write!(f, "-"),
      BinaryOperator::Mul => write!(f, "*"),
      BinaryOperator::Div => write!(f, "/"),
      BinaryOperator::Mod => write!(f, "%"),
      BinaryOperator::Pow => write!(f, "**"),
      BinaryOperator::Eq => write!(f, "=="),
      BinaryOperator::Ne => write!(f, "!="),
      BinaryOperator::Lt => write!(f, "<"),
      BinaryOperator::Le => write!(f, "<="),
      BinaryOperator::Gt => write!(f, ">"),
      BinaryOperator::Ge => write!(f, ">="),
      BinaryOperator::And => write!(f, "&&"),
      BinaryOperator::Or => write!(f, "||"),
    }
  }
}

/// Represents a unary operator
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
  /// Negation
  Neg,
  /// Logical NOT
  Not,
  /// Bitwise NOT
  BitwiseNot,
}

impl fmt::Display for UnaryOperator {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      UnaryOperator::Neg => write!(f, "-"),
      UnaryOperator::Not => write!(f, "!"),
      UnaryOperator::BitwiseNot => write!(f, "~"),
    }
  }
}

/// Represents a binding in a let expression
#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
  /// The pattern to bind
  pub pattern: Pattern,
  /// The expression to bind to
  pub expression: Expression,
  /// Optional type annotation
  pub type_annotation: Option<TypeExpr>,
  /// The source location span
  pub span: Span,
}

impl Binding {
  /// Create a new binding
  pub fn new(
    pattern: Pattern,
    expression: Expression,
    type_annotation: Option<TypeExpr>,
    span: Span,
  ) -> Self {
    Self {
      pattern,
      expression,
      type_annotation,
      span,
    }
  }
}

impl fmt::Display for Binding {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.type_annotation {
      Some(ty) => write!(f, "{}: {} = {}", self.pattern, ty, self.expression),
      None => write!(f, "{} = {}", self.pattern, self.expression),
    }
  }
}

/// Represents a match arm in a match expression
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
  /// The pattern to match
  pub pattern: Pattern,
  /// The expression to evaluate if the pattern matches
  pub expression: Expression,
  /// Optional guard condition
  pub guard: Option<Expression>,
  /// The source location span
  pub span: Span,
}

impl MatchArm {
  /// Create a new match arm
  pub fn new(
    pattern: Pattern,
    expression: Expression,
    guard: Option<Expression>,
    span: Span,
  ) -> Self {
    Self {
      pattern,
      expression,
      guard,
      span,
    }
  }
}

impl fmt::Display for MatchArm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.guard {
      Some(guard) => write!(f, "{} when {} => {}", self.pattern, guard, self.expression),
      None => write!(f, "{} => {}", self.pattern, self.expression),
    }
  }
}

/// Represents a handler case in an effect handler
#[derive(Debug, Clone, PartialEq)]
pub struct HandlerCase {
  /// The effect operation to handle
  pub operation: Identifier,
  /// The pattern for the operation arguments
  pub pattern: Pattern,
  /// The expression to evaluate when handling the effect
  pub expression: Expression,
  /// The source location span
  pub span: Span,
}

impl HandlerCase {
  /// Create a new handler case
  pub fn new(operation: Identifier, pattern: Pattern, expression: Expression, span: Span) -> Self {
    Self {
      operation,
      pattern,
      expression,
      span,
    }
  }
}

impl fmt::Display for HandlerCase {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{} {} => {}",
      self.operation, self.pattern, self.expression
    )
  }
}

/// Represents a statement in the AST
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
  /// Expression statement
  Expression {
    expression: Expression,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Let binding statement
  Let {
    binding: Binding,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Type declaration
  Type {
    name: Identifier,
    parameters: Vec<Identifier>,
    variants: Vec<TypeVariant>,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Effect declaration
  Effect {
    name: Identifier,
    operations: Vec<EffectOperation>,
    span: Span,
    type_annotation: Option<Type>,
  },
  /// Function declaration
  Function {
    name: Identifier,
    parameters: Vec<FunctionParameter>,
    return_type: Option<TypeExpr>,
    body: Expression,
    span: Span,
    type_annotation: Option<Type>,
  },
}

impl Statement {
  /// Get the span of the statement
  pub fn span(&self) -> Span {
    match self {
      Statement::Expression { span, .. } => *span,
      Statement::Let { span, .. } => *span,
      Statement::Type { span, .. } => *span,
      Statement::Effect { span, .. } => *span,
      Statement::Function { span, .. } => *span,
      _ => todo!(),
      _ => todo!(),
    }
  }

  /// Get the type annotation of the statement
  pub fn type_annotation(&self) -> Option<&Type> {
    match self {
      Statement::Expression {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Statement::Let {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Statement::Type {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Statement::Effect {
        type_annotation, ..
      } => type_annotation.as_ref(),
      Statement::Function {
        type_annotation, ..
      } => type_annotation.as_ref(),
    }
  }

  /// Set the type annotation of the statement
  pub fn set_type_annotation(&mut self, annotation: Option<Type>) {
    match self {
      Statement::Expression {
        type_annotation, ..
      } => *type_annotation = annotation,
      Statement::Let {
        type_annotation, ..
      } => *type_annotation = annotation,
      Statement::Type {
        type_annotation, ..
      } => *type_annotation = annotation,
      Statement::Effect {
        type_annotation, ..
      } => *type_annotation = annotation,
      Statement::Function {
        type_annotation, ..
      } => *type_annotation = annotation,
    }
  }
}

impl fmt::Display for Statement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Statement::Expression {
        type_annotation: None,
        expression,
        ..
      } => write!(f, "{}", expression),
      Statement::Let {
        type_annotation: None,
        binding,
        ..
      } => write!(f, "let {}", binding),
      Statement::Type {
        type_annotation: None,
        name,
        parameters,
        variants,
        ..
      } => {
        if parameters.is_empty() {
          write!(
            f,
            "type {} = {}",
            name,
            variants
              .iter()
              .map(|v| v.to_string())
              .collect::<Vec<_>>()
              .join(" | ")
          )
        } else {
          write!(
            f,
            "type {}<{}> = {}",
            name,
            parameters
              .iter()
              .map(|p| p.to_string())
              .collect::<Vec<_>>()
              .join(", "),
            variants
              .iter()
              .map(|v| v.to_string())
              .collect::<Vec<_>>()
              .join(" | ")
          )
        }
      }
      Statement::Effect {
        type_annotation: None,
        name,
        operations,
        ..
      } => {
        write!(
          f,
          "effect {} {{\n  {}\n}}",
          name,
          operations
            .iter()
            .map(|o| o.to_string())
            .collect::<Vec<_>>()
            .join("\n  ")
        )
      }
      Statement::Function {
        type_annotation: None,
        name,
        parameters,
        return_type,
        body,
        ..
      } => {
        let param_str = parameters
          .iter()
          .map(|p| p.to_string())
          .collect::<Vec<_>>()
          .join(" ");
        let return_str = return_type
          .as_ref()
          .map(|t| format!(" -> {}", t))
          .unwrap_or_default();
        write!(f, "fn {}({}){} = {}", name, param_str, return_str, body)
      }
      _ => todo!(),
    }
  }
}

/// Represents a type variant in a type declaration
#[derive(Debug, Clone, PartialEq)]
pub struct TypeVariant {
  /// The name of the variant
  pub name: Identifier,
  /// The fields of the variant (empty for unit variants)
  pub fields: Vec<TypeExpr>,
  /// The source location span
  pub span: Span,
}

impl TypeVariant {
  /// Create a new type variant
  pub fn new(name: Identifier, fields: Vec<TypeExpr>, span: Span) -> Self {
    Self { name, fields, span }
  }
}

impl fmt::Display for TypeVariant {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.fields.is_empty() {
      write!(f, "{}", self.name)
    } else {
      write!(
        f,
        "{}({})",
        self.name,
        self
          .fields
          .iter()
          .map(|t| t.to_string())
          .collect::<Vec<_>>()
          .join(", ")
      )
    }
  }
}

/// Represents an effect operation in an effect declaration
#[derive(Debug, Clone, PartialEq)]
pub struct EffectOperation {
  /// The name of the operation
  pub name: Identifier,
  /// The input type of the operation
  pub input_type: TypeExpr,
  /// The output type of the operation
  pub output_type: TypeExpr,
  /// The source location span
  pub span: Span,
}

impl EffectOperation {
  /// Create a new effect operation
  pub fn new(name: Identifier, input_type: TypeExpr, output_type: TypeExpr, span: Span) -> Self {
    Self {
      name,
      input_type,
      output_type,
      span,
    }
  }
}

impl fmt::Display for EffectOperation {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}: {} -> {}",
      self.name, self.input_type, self.output_type
    )
  }
}

/// Represents a function parameter
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameter {
  /// The pattern for the parameter
  pub pattern: Pattern,
  /// Optional type annotation
  pub type_annotation: Option<TypeExpr>,
  /// The source location span
  pub span: Span,
}

impl FunctionParameter {
  /// Create a new function parameter
  pub fn new(pattern: Pattern, type_annotation: Option<TypeExpr>, span: Span) -> Self {
    Self {
      pattern,
      type_annotation,
      span,
    }
  }
}

impl fmt::Display for FunctionParameter {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.type_annotation {
      Some(ty) => write!(f, "{}: {}", self.pattern, ty),
      None => write!(f, "{}", self.pattern),
    }
  }
}

/// Represents a declaration in the AST
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
  /// Type declaration
  Type {
    name: Identifier,
    parameters: Vec<Identifier>,
    variants: Vec<TypeVariant>,
    span: Span,
  },
  /// Effect declaration
  Effect {
    name: Identifier,
    operations: Vec<EffectOperation>,
    span: Span,
  },
  /// Function declaration
  Function {
    name: Identifier,
    parameters: Vec<FunctionParameter>,
    return_type: Option<TypeExpr>,
    body: Expression,
    span: Span,
  },
}

impl Declaration {
  /// Get the span of the declaration
  pub fn span(&self) -> Span {
    match self {
      Declaration::Type { span, .. } => *span,
      Declaration::Effect { span, .. } => *span,
      Declaration::Function { span, .. } => *span,
    }
  }
}

impl fmt::Display for Declaration {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Declaration::Type {
        name,
        parameters,
        variants,
        ..
      } => {
        if parameters.is_empty() {
          write!(
            f,
            "type {} = {}",
            name,
            variants
              .iter()
              .map(|v| v.to_string())
              .collect::<Vec<_>>()
              .join(" | ")
          )
        } else {
          write!(
            f,
            "type {}<{}> = {}",
            name,
            parameters
              .iter()
              .map(|p| p.to_string())
              .collect::<Vec<_>>()
              .join(", "),
            variants
              .iter()
              .map(|v| v.to_string())
              .collect::<Vec<_>>()
              .join(" | ")
          )
        }
      }
      Declaration::Effect {
        name, operations, ..
      } => {
        write!(
          f,
          "effect {} {{\n  {}\n}}",
          name,
          operations
            .iter()
            .map(|o| o.to_string())
            .collect::<Vec<_>>()
            .join("\n  ")
        )
      }
      Declaration::Function {
        name,
        parameters,
        return_type,
        body,
        ..
      } => {
        let param_str = parameters
          .iter()
          .map(|p| p.to_string())
          .collect::<Vec<_>>()
          .join(" ");
        let return_str = return_type
          .as_ref()
          .map(|t| format!(" -> {}", t))
          .unwrap_or_default();
        write!(f, "fn {}({}){} = {}", name, param_str, return_str, body)
      }
    }
  }
}

/// Represents the root node of the AST
#[derive(Debug, Clone, PartialEq)]
pub struct AstNode {
  /// The statements in the program
  pub statements: Vec<Statement>,
  /// The source location span
  pub span: Span,
}

impl AstNode {
  /// Create a new AST node
  pub fn new(statements: Vec<Statement>, span: Span) -> Self {
    Self { statements, span }
  }

  /// Create an empty AST node
  pub fn empty() -> Self {
    Self {
      statements: Vec::new(),
      span: Span::new(SourceLocation::start(), SourceLocation::start()),
    }
  }

  /// Add a statement to the AST
  pub fn add_statement(&mut self, statement: Statement) {
    let statement_span = statement.span();
    self.statements.push(statement);
    // Update the span to include the new statement
    if let Some(span) = Span::from_spans(&[self.span, statement_span]) {
      self.span = span;
    }
  }

  /// Get the span of the AST
  pub fn span(&self) -> Span {
    self.span
  }
}

impl fmt::Display for AstNode {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (i, statement) in self.statements.iter().enumerate() {
      if i > 0 {
        writeln!(f)?;
      }
      write!(f, "{}", statement)?;
    }
    Ok(())
  }
}

/// Trait for visiting AST nodes
pub trait AstVisitor {
  /// Visit an AST node
  fn visit_ast(&mut self, ast: &AstNode);
  /// Visit a statement
  fn visit_statement(&mut self, statement: &Statement);
  /// Visit an expression
  fn visit_expression(&mut self, expression: &Expression);
  /// Visit a pattern
  fn visit_pattern(&mut self, pattern: &Pattern);
  /// Visit a type expression
  fn visit_type_expr(&mut self, type_expr: &TypeExpr);
  /// Visit a declaration
  fn visit_declaration(&mut self, declaration: &Declaration);
}

/// Default implementation of AstVisitor that traverses the entire AST
impl<T> AstVisitor for T
where
  T: crate::parser::visitor::Visitor,
{
  fn visit_ast(&mut self, ast: &AstNode) {
    for statement in &ast.statements {
      self.visit_statement(statement);
    }
  }

  fn visit_statement(&mut self, statement: &Statement) {
    match statement {
      Statement::Expression {
        type_annotation: None,
        expression,
        ..
      } => {
        self.visit_expression(expression);
      }
      Statement::Let {
        type_annotation: None,
        binding,
        ..
      } => {
        self.visit_pattern(&binding.pattern);
        self.visit_expression(&binding.expression);
        if let Some(ty) = &binding.type_annotation {
          self.visit_type_expr(ty);
        }
      }
      Statement::Type {
        type_annotation: None,
        variants,
        ..
      } => {
        for variant in variants {
          for field in &variant.fields {
            self.visit_type_expr(field);
          }
        }
      }
      Statement::Effect {
        type_annotation: None,
        operations,
        ..
      } => {
        for operation in operations {
          self.visit_type_expr(&operation.input_type);
          self.visit_type_expr(&operation.output_type);
        }
      }
      Statement::Function {
        type_annotation: None,
        parameters,
        return_type,
        body,
        ..
      } => {
        for param in parameters {
          self.visit_pattern(&param.pattern);
          if let Some(ty) = &param.type_annotation {
            self.visit_type_expr(ty);
          }
        }
        if let Some(ty) = return_type {
          self.visit_type_expr(ty);
        }
        self.visit_expression(body);
      }
      _ => todo!(),
    }
  }

  fn visit_expression(&mut self, expression: &Expression) {
    match expression {
      Expression::Application {
        type_annotation: None,
        function,
        arguments,
        ..
      } => {
        self.visit_expression(function);
        for arg in arguments {
          self.visit_expression(arg);
        }
      }
      Expression::BinaryOp {
        type_annotation: None,
        left,
        right,
        ..
      } => {
        self.visit_expression(left);
        self.visit_expression(right);
      }
      Expression::UnaryOp {
        type_annotation: None,
        operand,
        ..
      } => {
        self.visit_expression(operand);
      }
      Expression::Let {
        type_annotation: None,
        bindings,
        body,
        ..
      } => {
        for binding in bindings {
          self.visit_pattern(&binding.pattern);
          self.visit_expression(&binding.expression);
          if let Some(ty) = &binding.type_annotation {
            self.visit_type_expr(ty);
          }
        }
        self.visit_expression(body);
      }
      Expression::If {
        type_annotation: None,
        condition,
        then_branch,
        else_branch,
        ..
      } => {
        self.visit_expression(condition);
        self.visit_expression(then_branch);
        self.visit_expression(else_branch);
      }
      Expression::Match {
        type_annotation: None,
        scrutinee,
        arms,
        ..
      } => {
        self.visit_expression(scrutinee);
        for arm in arms {
          self.visit_pattern(&arm.pattern);
          self.visit_expression(&arm.expression);
          if let Some(guard) = &arm.guard {
            self.visit_expression(guard);
          }
        }
      }
      Expression::Lambda {
        type_annotation: None,
        parameters,
        body,
        ..
      } => {
        for param in parameters {
          self.visit_pattern(param);
        }
        self.visit_expression(body);
      }
      Expression::Tuple {
        type_annotation: None,
        elements,
        ..
      } => {
        for element in elements {
          self.visit_expression(element);
        }
      }
      Expression::List {
        type_annotation: None,
        elements,
        ..
      } => {
        for element in elements {
          self.visit_expression(element);
        }
      }
      Expression::EffectOp {
        type_annotation: None,
        arguments,
        ..
      } => {
        for arg in arguments {
          self.visit_expression(arg);
        }
      }
      Expression::Handler {
        type_annotation: None,
        expression,
        cases,
        ..
      } => {
        self.visit_expression(expression);
        for case in cases {
          self.visit_pattern(&case.pattern);
          self.visit_expression(&case.expression);
        }
      }
      _ => {} // Literal, Variable don't have sub-expressions
    }
  }

  fn visit_pattern(&mut self, pattern: &Pattern) {
    match pattern {
      Pattern::Constructor { arguments, .. } => {
        for arg in arguments {
          self.visit_pattern(arg);
        }
      }
      Pattern::Tuple { elements, .. } => {
        for element in elements {
          self.visit_pattern(element);
        }
      }
      Pattern::List { elements, .. } => {
        for element in elements {
          self.visit_pattern(element);
        }
      }
      Pattern::Or { left, right, .. } => {
        self.visit_pattern(left);
        self.visit_pattern(right);
      }
      Pattern::As { pattern, .. } => {
        self.visit_pattern(pattern);
      }
      _ => {} // Variable, Wildcard, Literal don't have sub-patterns
    }
  }

  fn visit_type_expr(&mut self, type_expr: &TypeExpr) {
    match type_expr {
      TypeExpr::Function {
        parameter,
        return_type,
        ..
      } => {
        self.visit_type_expr(parameter);
        self.visit_type_expr(return_type);
      }
      TypeExpr::Tuple { elements, .. } => {
        for element in elements {
          self.visit_type_expr(element);
        }
      }
      TypeExpr::List { element_type, .. } => {
        self.visit_type_expr(element_type);
      }
      TypeExpr::Generic { arguments, .. } => {
        for arg in arguments {
          self.visit_type_expr(arg);
        }
      }
      TypeExpr::Effect {
        input_type,
        output_type,
        ..
      } => {
        self.visit_type_expr(input_type);
        self.visit_type_expr(output_type);
      }
      _ => {} // Variable, Constructor don't have sub-types
    }
  }

  fn visit_declaration(&mut self, declaration: &Declaration) {
    match declaration {
      Declaration::Type { variants, .. } => {
        for variant in variants {
          for field in &variant.fields {
            self.visit_type_expr(field);
          }
        }
      }
      Declaration::Effect { operations, .. } => {
        for operation in operations {
          self.visit_type_expr(&operation.input_type);
          self.visit_type_expr(&operation.output_type);
        }
      }
      Declaration::Function {
        parameters,
        return_type,
        body,
        ..
      } => {
        for param in parameters {
          self.visit_pattern(&param.pattern);
          if let Some(ty) = &param.type_annotation {
            self.visit_type_expr(ty);
          }
        }
        if let Some(ty) = return_type {
          self.visit_type_expr(ty);
        }
        self.visit_expression(body);
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::{SourceLocation, Token, TokenKind};

  #[test]
  fn test_span_creation() {
    let start = SourceLocation::new(1, 1, 0);
    let end = SourceLocation::new(1, 5, 4);
    let span = Span::new(start, end);

    assert_eq!(span.start, start);
    assert_eq!(span.end, end);
  }

  #[test]
  fn test_span_from_token() {
    let token = Token::new(
      TokenKind::Let,
      "let".to_string(),
      SourceLocation::new(1, 1, 0),
      SourceLocation::new(1, 4, 3),
    );
    let span = Span::from_token(&token);

    assert_eq!(span.start, token.start);
    assert_eq!(span.end, token.end);
  }

  #[test]
  fn test_identifier_creation() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 4, 3));
    let id = Identifier::new("x".to_string(), span);

    assert_eq!(id.name, "x");
    assert_eq!(id.span, span);
  }

  #[test]
  fn test_literal_span() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 2, 1));
    let literal = Literal::Integer { value: 42, span };

    assert_eq!(literal.span(), span);
  }

  #[test]
  fn test_pattern_span() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let pattern = Pattern::Variable {
      identifier: Identifier::new("x".to_string(), span),
      span,
    };

    assert_eq!(pattern.span(), span);
  }

  #[test]
  fn test_expression_span() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let expr = Expression::Literal {
      type_annotation: None,
      literal: Literal::Integer { value: 42, span },
      span,
    };

    assert_eq!(expr.span(), span);
  }

  #[test]
  fn test_statement_span() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let stmt = Statement::Expression {
      type_annotation: None,
      expression: Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 42, span },
        span,
      },
      span,
    };

    assert_eq!(stmt.span(), span);
  }

  #[test]
  fn test_ast_node_creation() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let ast = AstNode::new(Vec::new(), span);

    assert_eq!(ast.statements.len(), 0);
    assert_eq!(ast.span(), span);
  }

  #[test]
  fn test_ast_node_add_statement() {
    let mut ast = AstNode::empty();
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let stmt = Statement::Expression {
      type_annotation: None,
      expression: Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 42, span },
        span,
      },
      span,
    };

    ast.add_statement(stmt);
    assert_eq!(ast.statements.len(), 1);
  }

  #[test]
  fn test_binary_operator_display() {
    assert_eq!(BinaryOperator::Add.to_string(), "+");
    assert_eq!(BinaryOperator::Mul.to_string(), "*");
    assert_eq!(BinaryOperator::Eq.to_string(), "==");
  }

  #[test]
  fn test_unary_operator_display() {
    assert_eq!(UnaryOperator::Neg.to_string(), "-");
    assert_eq!(UnaryOperator::Not.to_string(), "!");
  }

  #[test]
  fn test_literal_display() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let int_lit = Literal::Integer { value: 42, span };
    let str_lit = Literal::String {
      value: "hello".to_string(),
      span,
    };

    assert_eq!(int_lit.to_string(), "42");
    assert_eq!(str_lit.to_string(), "\"hello\"");
  }

  #[test]
  fn test_pattern_display() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let var_pattern = Pattern::Variable {
      identifier: Identifier::new("x".to_string(), span),
      span,
    };
    let wildcard_pattern = Pattern::Wildcard { span };

    assert_eq!(var_pattern.to_string(), "x");
    assert_eq!(wildcard_pattern.to_string(), "_");
  }

  #[test]
  fn test_expression_display() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let var_expr = Expression::Variable {
      type_annotation: None,
      identifier: Identifier::new("x".to_string(), span),
      span,
    };
    let bin_op = Expression::BinaryOp {
      type_annotation: None,
      left: Box::new(var_expr.clone()),
      operator: BinaryOperator::Add,
      right: Box::new(Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 1, span },
        span,
      }),
      span,
    };

    assert_eq!(var_expr.to_string(), "x");
    assert_eq!(bin_op.to_string(), "x + 1");
  }

  // Additional comprehensive tests for 100% coverage

  #[test]
  fn test_span_from_spans() {
    let span1 = Span::new(SourceLocation::new(1, 1, 0), SourceLocation::new(1, 5, 4));
    let span2 = Span::new(SourceLocation::new(2, 1, 5), SourceLocation::new(2, 10, 14));
    let span3 = Span::new(SourceLocation::new(1, 3, 2), SourceLocation::new(1, 7, 6));

    let combined = Span::from_spans(&[span1, span2, span3]).unwrap();
    assert_eq!(combined.start, SourceLocation::new(1, 1, 0));
    assert_eq!(combined.end, SourceLocation::new(2, 10, 14));

    // Test empty spans
    assert!(Span::from_spans(&[]).is_none());
  }

  #[test]
  fn test_span_extend() {
    let mut span = Span::new(SourceLocation::new(1, 5, 4), SourceLocation::new(1, 10, 9));
    let other = Span::new(SourceLocation::new(1, 1, 0), SourceLocation::new(1, 15, 14));

    span.extend(&other);
    assert_eq!(span.start, SourceLocation::new(1, 1, 0));
    assert_eq!(span.end, SourceLocation::new(1, 15, 14));
  }

  #[test]
  fn test_span_display() {
    let span = Span::new(SourceLocation::new(1, 1, 0), SourceLocation::new(1, 5, 4));
    assert_eq!(span.to_string(), "1:1:1:5");
  }

  #[test]
  fn test_identifier_from_token() {
    let token = Token::new(
      TokenKind::Identifier,
      "x".to_string(),
      SourceLocation::new(1, 1, 0),
      SourceLocation::new(1, 2, 1),
    );
    let id = Identifier::from_token(&token);

    assert_eq!(id.name, "x");
    assert_eq!(id.span, Span::from_token(&token));
  }

  #[test]
  fn test_identifier_display() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 4, 3));
    let id = Identifier::new("test_var".to_string(), span);
    assert_eq!(id.to_string(), "test_var");
  }

  #[test]
  fn test_literal_all_variants() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 2, 1));

    let int_lit = Literal::Integer { value: 42, span };
    let float_lit = Literal::Float { value: 3.14, span };
    let str_lit = Literal::String {
      value: "hello".to_string(),
      span,
    };
    let char_lit = Literal::Char { value: 'a', span };
    let bool_lit = Literal::Boolean { value: true, span };
    let unit_lit = Literal::Unit { span };

    assert_eq!(int_lit.span(), span);
    assert_eq!(float_lit.span(), span);
    assert_eq!(str_lit.span(), span);
    assert_eq!(char_lit.span(), span);
    assert_eq!(bool_lit.span(), span);
    assert_eq!(unit_lit.span(), span);
  }

  #[test]
  fn test_literal_display_all_variants() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 2, 1));

    let int_lit = Literal::Integer { value: 42, span };
    let float_lit = Literal::Float { value: 3.14, span };
    let str_lit = Literal::String {
      value: "hello".to_string(),
      span,
    };
    let char_lit = Literal::Char { value: 'a', span };
    let bool_lit = Literal::Boolean { value: true, span };
    let unit_lit = Literal::Unit { span };

    assert_eq!(int_lit.to_string(), "42");
    assert_eq!(float_lit.to_string(), "3.14");
    assert_eq!(str_lit.to_string(), "\"hello\"");
    assert_eq!(char_lit.to_string(), "'a'");
    assert_eq!(bool_lit.to_string(), "true");
    assert_eq!(unit_lit.to_string(), "()");
  }

  #[test]
  fn test_pattern_all_variants() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let id = Identifier::new("x".to_string(), span);

    let var_pattern = Pattern::Variable {
      identifier: id.clone(),
      span,
    };
    let wildcard_pattern = Pattern::Wildcard { span };
    let literal_pattern = Pattern::Literal {
      literal: Literal::Integer { value: 42, span },
      span,
    };
    let constructor_pattern = Pattern::Constructor {
      constructor: id.clone(),
      arguments: vec![],
      span,
    };
    let tuple_pattern = Pattern::Tuple {
      elements: vec![],
      span,
    };
    let list_pattern = Pattern::List {
      elements: vec![],
      span,
    };
    let or_pattern = Pattern::Or {
      left: Box::new(var_pattern.clone()),
      right: Box::new(wildcard_pattern.clone()),
      span,
    };
    let as_pattern = Pattern::As {
      pattern: Box::new(var_pattern.clone()),
      identifier: id.clone(),
      span,
    };

    assert_eq!(var_pattern.span(), span);
    assert_eq!(wildcard_pattern.span(), span);
    assert_eq!(literal_pattern.span(), span);
    assert_eq!(constructor_pattern.span(), span);
    assert_eq!(tuple_pattern.span(), span);
    assert_eq!(list_pattern.span(), span);
    assert_eq!(or_pattern.span(), span);
    assert_eq!(as_pattern.span(), span);
  }

  #[test]
  fn test_pattern_display_all_variants() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let id = Identifier::new("x".to_string(), span);

    let var_pattern = Pattern::Variable {
      identifier: id.clone(),
      span,
    };
    let wildcard_pattern = Pattern::Wildcard { span };
    let literal_pattern = Pattern::Literal {
      literal: Literal::Integer { value: 42, span },
      span,
    };
    let constructor_pattern = Pattern::Constructor {
      constructor: id.clone(),
      arguments: vec![],
      span,
    };
    let tuple_pattern = Pattern::Tuple {
      elements: vec![],
      span,
    };
    let list_pattern = Pattern::List {
      elements: vec![],
      span,
    };
    let or_pattern = Pattern::Or {
      left: Box::new(var_pattern.clone()),
      right: Box::new(wildcard_pattern.clone()),
      span,
    };
    let as_pattern = Pattern::As {
      pattern: Box::new(var_pattern.clone()),
      identifier: id.clone(),
      span,
    };

    assert_eq!(var_pattern.to_string(), "x");
    assert_eq!(wildcard_pattern.to_string(), "_");
    assert_eq!(literal_pattern.to_string(), "42");
    assert_eq!(constructor_pattern.to_string(), "x");
    assert_eq!(tuple_pattern.to_string(), "()");
    assert_eq!(list_pattern.to_string(), "[]");
    assert_eq!(or_pattern.to_string(), "x | _");
    assert_eq!(as_pattern.to_string(), "x as x");
  }

  #[test]
  fn test_pattern_with_arguments() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let id = Identifier::new("x".to_string(), span);
    let arg_pattern = Pattern::Variable {
      identifier: id.clone(),
      span,
    };

    let constructor_pattern = Pattern::Constructor {
      constructor: id.clone(),
      arguments: vec![arg_pattern.clone()],
      span,
    };
    let tuple_pattern = Pattern::Tuple {
      elements: vec![arg_pattern.clone(), arg_pattern.clone()],
      span,
    };
    let list_pattern = Pattern::List {
      elements: vec![arg_pattern.clone()],
      span,
    };

    assert_eq!(constructor_pattern.to_string(), "x(x)");
    assert_eq!(tuple_pattern.to_string(), "(x, x)");
    assert_eq!(list_pattern.to_string(), "[x]");
  }

  #[test]
  fn test_type_expr_all_variants() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let id = Identifier::new("Int".to_string(), span);

    let var_type = TypeExpr::Variable {
      name: "a".to_string(),
      span,
    };
    let constructor_type = TypeExpr::Constructor {
      name: id.clone(),
      span,
    };
    let function_type = TypeExpr::Function {
      parameter: Box::new(var_type.clone()),
      return_type: Box::new(constructor_type.clone()),
      span,
    };
    let tuple_type = TypeExpr::Tuple {
      elements: vec![],
      span,
    };
    let list_type = TypeExpr::List {
      element_type: Box::new(constructor_type.clone()),
      span,
    };
    let generic_type = TypeExpr::Generic {
      constructor: id.clone(),
      arguments: vec![],
      span,
    };
    let effect_type = TypeExpr::Effect {
      input_type: Box::new(var_type.clone()),
      output_type: Box::new(constructor_type.clone()),
      span,
    };

    assert_eq!(var_type.span(), span);
    assert_eq!(constructor_type.span(), span);
    assert_eq!(function_type.span(), span);
    assert_eq!(tuple_type.span(), span);
    assert_eq!(list_type.span(), span);
    assert_eq!(generic_type.span(), span);
    assert_eq!(effect_type.span(), span);
  }

  #[test]
  fn test_type_expr_display_all_variants() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let id = Identifier::new("Int".to_string(), span);

    let var_type = TypeExpr::Variable {
      name: "a".to_string(),
      span,
    };
    let constructor_type = TypeExpr::Constructor {
      name: id.clone(),
      span,
    };
    let function_type = TypeExpr::Function {
      parameter: Box::new(var_type.clone()),
      return_type: Box::new(constructor_type.clone()),
      span,
    };
    let tuple_type = TypeExpr::Tuple {
      elements: vec![],
      span,
    };
    let list_type = TypeExpr::List {
      element_type: Box::new(constructor_type.clone()),
      span,
    };
    let generic_type = TypeExpr::Generic {
      constructor: id.clone(),
      arguments: vec![],
      span,
    };
    let effect_type = TypeExpr::Effect {
      input_type: Box::new(var_type.clone()),
      output_type: Box::new(constructor_type.clone()),
      span,
    };

    assert_eq!(var_type.to_string(), "'a");
    assert_eq!(constructor_type.to_string(), "Int");
    assert_eq!(function_type.to_string(), "'a -> Int");
    assert_eq!(tuple_type.to_string(), "()");
    assert_eq!(list_type.to_string(), "[Int]");
    assert_eq!(generic_type.to_string(), "Int<>");
    assert_eq!(effect_type.to_string(), "'a !> Int");
  }

  #[test]
  fn test_type_expr_with_arguments() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let int_type = TypeExpr::Constructor {
      name: Identifier::new("Int".to_string(), span),
      span,
    };
    let str_type = TypeExpr::Constructor {
      name: Identifier::new("String".to_string(), span),
      span,
    };

    let tuple_type = TypeExpr::Tuple {
      elements: vec![int_type.clone(), str_type.clone()],
      span,
    };
    let generic_type = TypeExpr::Generic {
      constructor: Identifier::new("Option".to_string(), span),
      arguments: vec![int_type.clone()],
      span,
    };

    assert_eq!(tuple_type.to_string(), "(Int, String)");
    assert_eq!(generic_type.to_string(), "Option<Int>");
  }

  #[test]
  fn test_expression_all_variants() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let id = Identifier::new("x".to_string(), span);

    let literal_expr = Expression::Literal {
      type_annotation: None,
      literal: Literal::Integer { value: 42, span },
      span,
    };
    let var_expr = Expression::Variable {
      type_annotation: None,
      identifier: id.clone(),
      span,
    };
    let app_expr = Expression::Application {
      type_annotation: None,
      function: Box::new(var_expr.clone()),
      arguments: vec![],
      span,
    };
    let bin_op_expr = Expression::BinaryOp {
      type_annotation: None,
      left: Box::new(var_expr.clone()),
      operator: BinaryOperator::Add,
      right: Box::new(literal_expr.clone()),
      span,
    };
    let unary_op_expr = Expression::UnaryOp {
      type_annotation: None,
      operator: UnaryOperator::Neg,
      operand: Box::new(var_expr.clone()),
      span,
    };
    let if_expr = Expression::If {
      type_annotation: None,
      condition: Box::new(var_expr.clone()),
      then_branch: Box::new(literal_expr.clone()),
      else_branch: Box::new(literal_expr.clone()),
      span,
    };
    let match_expr = Expression::Match {
      type_annotation: None,
      scrutinee: Box::new(var_expr.clone()),
      arms: vec![],
      span,
    };
    let lambda_expr = Expression::Lambda {
      type_annotation: None,
      parameters: vec![],
      body: Box::new(literal_expr.clone()),
      span,
    };
    let tuple_expr = Expression::Tuple {
      type_annotation: None,
      elements: vec![],
      span,
    };
    let list_expr = Expression::List {
      type_annotation: None,
      elements: vec![],
      span,
    };
    let effect_op_expr = Expression::EffectOp {
      type_annotation: None,
      operation: id.clone(),
      arguments: vec![],
      span,
    };
    let handler_expr = Expression::Handler {
      type_annotation: None,
      expression: Box::new(literal_expr.clone()),
      cases: vec![],
      span,
    };

    assert_eq!(literal_expr.span(), span);
    assert_eq!(var_expr.span(), span);
    assert_eq!(app_expr.span(), span);
    assert_eq!(bin_op_expr.span(), span);
    assert_eq!(unary_op_expr.span(), span);
    assert_eq!(if_expr.span(), span);
    assert_eq!(match_expr.span(), span);
    assert_eq!(lambda_expr.span(), span);
    assert_eq!(tuple_expr.span(), span);
    assert_eq!(list_expr.span(), span);
    assert_eq!(effect_op_expr.span(), span);
    assert_eq!(handler_expr.span(), span);
  }

  #[test]
  fn test_expression_display_complex() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let id = Identifier::new("x".to_string(), span);

    let app_expr = Expression::Application {
      type_annotation: None,
      function: Box::new(Expression::Variable {
        type_annotation: None,
        identifier: id.clone(),
        span,
      }),
      arguments: vec![
        Expression::Literal {
          type_annotation: None,
          literal: Literal::Integer { value: 1, span },
          span,
        },
        Expression::Literal {
          type_annotation: None,
          literal: Literal::Integer { value: 2, span },
          span,
        },
      ],
      span,
    };
    let let_expr = Expression::Let {
      type_annotation: None,
      bindings: vec![Binding::new(
        Pattern::Variable {
          identifier: id.clone(),
          span,
        },
        Expression::Literal {
          type_annotation: None,
          literal: Literal::Integer { value: 42, span },
          span,
        },
        None,
        span,
      )],
      body: Box::new(Expression::Variable {
        type_annotation: None,
        identifier: id.clone(),
        span,
      }),
      span,
    };
    let match_expr = Expression::Match {
      type_annotation: None,
      scrutinee: Box::new(Expression::Variable {
        type_annotation: None,
        identifier: id.clone(),
        span,
      }),
      arms: vec![MatchArm::new(
        Pattern::Variable {
          identifier: id.clone(),
          span,
        },
        Expression::Literal {
          type_annotation: None,
          literal: Literal::Integer { value: 0, span },
          span,
        },
        None,
        span,
      )],
      span,
    };
    let lambda_expr = Expression::Lambda {
      type_annotation: None,
      parameters: vec![
        Pattern::Variable {
          identifier: id.clone(),
          span,
        },
        Pattern::Variable {
          identifier: id.clone(),
          span,
        },
      ],
      body: Box::new(Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 0, span },
        span,
      }),
      span,
    };
    let tuple_expr = Expression::Tuple {
      type_annotation: None,
      elements: vec![
        Expression::Literal {
          type_annotation: None,
          literal: Literal::Integer { value: 1, span },
          span,
        },
        Expression::Literal {
          type_annotation: None,
          literal: Literal::Integer { value: 2, span },
          span,
        },
      ],
      span,
    };
    let list_expr = Expression::List {
      type_annotation: None,
      elements: vec![
        Expression::Literal {
          type_annotation: None,
          literal: Literal::Integer { value: 1, span },
          span,
        },
        Expression::Literal {
          type_annotation: None,
          literal: Literal::Integer { value: 2, span },
          span,
        },
      ],
      span,
    };
    let effect_op_expr = Expression::EffectOp {
      type_annotation: None,
      operation: id.clone(),
      arguments: vec![Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 42, span },
        span,
      }],
      span,
    };
    let handler_expr = Expression::Handler {
      type_annotation: None,
      expression: Box::new(Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 0, span },
        span,
      }),
      cases: vec![HandlerCase::new(
        id.clone(),
        Pattern::Variable {
          identifier: id.clone(),
          span,
        },
        Expression::Literal {
          type_annotation: None,
          literal: Literal::Integer { value: 42, span },
          span,
        },
        span,
      )],
      span,
    };

    assert_eq!(app_expr.to_string(), "x(1, 2)");
    assert_eq!(let_expr.to_string(), "let x = 42 in x");
    assert_eq!(match_expr.to_string(), "match x with x => 0");
    assert_eq!(lambda_expr.to_string(), "\\x x -> 0");
    assert_eq!(tuple_expr.to_string(), "(1, 2)");
    assert_eq!(list_expr.to_string(), "[1, 2]");
    assert_eq!(effect_op_expr.to_string(), "do x(42)");
    assert_eq!(handler_expr.to_string(), "handle 0 with x x => 42");
  }

  #[test]
  fn test_binary_operator_all_variants() {
    assert_eq!(BinaryOperator::Add.to_string(), "+");
    assert_eq!(BinaryOperator::Sub.to_string(), "-");
    assert_eq!(BinaryOperator::Mul.to_string(), "*");
    assert_eq!(BinaryOperator::Div.to_string(), "/");
    assert_eq!(BinaryOperator::Mod.to_string(), "%");
    assert_eq!(BinaryOperator::Pow.to_string(), "**");
    assert_eq!(BinaryOperator::Eq.to_string(), "==");
    assert_eq!(BinaryOperator::Ne.to_string(), "!=");
    assert_eq!(BinaryOperator::Lt.to_string(), "<");
    assert_eq!(BinaryOperator::Le.to_string(), "<=");
    assert_eq!(BinaryOperator::Gt.to_string(), ">");
    assert_eq!(BinaryOperator::Ge.to_string(), ">=");
    assert_eq!(BinaryOperator::And.to_string(), "&&");
    assert_eq!(BinaryOperator::Or.to_string(), "||");
  }

  #[test]
  fn test_unary_operator_all_variants() {
    assert_eq!(UnaryOperator::Neg.to_string(), "-");
    assert_eq!(UnaryOperator::Not.to_string(), "!");
    assert_eq!(UnaryOperator::BitwiseNot.to_string(), "~");
  }

  #[test]
  fn test_binding_creation() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let pattern = Pattern::Variable {
      identifier: Identifier::new("x".to_string(), span),
      span,
    };
    let expression = Expression::Literal {
      type_annotation: None,
      literal: Literal::Integer { value: 42, span },
      span,
    };
    let type_annotation = Some(TypeExpr::Constructor {
      name: Identifier::new("Int".to_string(), span),
      span,
    });

    let binding = Binding::new(
      pattern.clone(),
      expression.clone(),
      type_annotation.clone(),
      span,
    );

    assert_eq!(binding.pattern, pattern);
    assert_eq!(binding.expression, expression);
    assert_eq!(binding.type_annotation, type_annotation);
    assert_eq!(binding.span, span);
  }

  #[test]
  fn test_binding_display() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let pattern = Pattern::Variable {
      identifier: Identifier::new("x".to_string(), span),
      span,
    };
    let expression = Expression::Literal {
      type_annotation: None,
      literal: Literal::Integer { value: 42, span },
      span,
    };

    let binding_without_type = Binding::new(pattern.clone(), expression.clone(), None, span);
    let binding_with_type = Binding::new(
      pattern.clone(),
      expression.clone(),
      Some(TypeExpr::Constructor {
        name: Identifier::new("Int".to_string(), span),
        span,
      }),
      span,
    );

    assert_eq!(binding_without_type.to_string(), "x = 42");
    assert_eq!(binding_with_type.to_string(), "x: Int = 42");
  }

  #[test]
  fn test_match_arm_creation() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let pattern = Pattern::Variable {
      identifier: Identifier::new("x".to_string(), span),
      span,
    };
    let expression = Expression::Literal {
      type_annotation: None,
      literal: Literal::Integer { value: 42, span },
      span,
    };
    let guard = Some(Expression::BinaryOp {
      type_annotation: None,
      left: Box::new(Expression::Variable {
        type_annotation: None,
        identifier: Identifier::new("x".to_string(), span),
        span,
      }),
      operator: BinaryOperator::Gt,
      right: Box::new(Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 0, span },
        span,
      }),
      span,
    });

    let match_arm = MatchArm::new(pattern.clone(), expression.clone(), guard.clone(), span);

    assert_eq!(match_arm.pattern, pattern);
    assert_eq!(match_arm.expression, expression);
    assert_eq!(match_arm.guard, guard);
    assert_eq!(match_arm.span, span);
  }

  #[test]
  fn test_match_arm_display() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let pattern = Pattern::Variable {
      identifier: Identifier::new("x".to_string(), span),
      span,
    };
    let expression = Expression::Literal {
      type_annotation: None,
      literal: Literal::Integer { value: 42, span },
      span,
    };

    let match_arm_without_guard = MatchArm::new(pattern.clone(), expression.clone(), None, span);
    let match_arm_with_guard = MatchArm::new(
      pattern.clone(),
      expression.clone(),
      Some(Expression::BinaryOp {
        type_annotation: None,
        left: Box::new(Expression::Variable {
          type_annotation: None,
          identifier: Identifier::new("x".to_string(), span),
          span,
        }),
        operator: BinaryOperator::Gt,
        right: Box::new(Expression::Literal {
          type_annotation: None,
          literal: Literal::Integer { value: 0, span },
          span,
        }),
        span,
      }),
      span,
    );

    assert_eq!(match_arm_without_guard.to_string(), "x => 42");
    assert_eq!(match_arm_with_guard.to_string(), "x when x > 0 => 42");
  }

  #[test]
  fn test_handler_case_creation() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let operation = Identifier::new("Get".to_string(), span);
    let pattern = Pattern::Variable {
      identifier: Identifier::new("x".to_string(), span),
      span,
    };
    let expression = Expression::Literal {
      type_annotation: None,
      literal: Literal::Integer { value: 42, span },
      span,
    };

    let handler_case =
      HandlerCase::new(operation.clone(), pattern.clone(), expression.clone(), span);

    assert_eq!(handler_case.operation, operation);
    assert_eq!(handler_case.pattern, pattern);
    assert_eq!(handler_case.expression, expression);
    assert_eq!(handler_case.span, span);
  }

  #[test]
  fn test_handler_case_display() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let operation = Identifier::new("Get".to_string(), span);
    let pattern = Pattern::Variable {
      identifier: Identifier::new("x".to_string(), span),
      span,
    };
    let expression = Expression::Literal {
      type_annotation: None,
      literal: Literal::Integer { value: 42, span },
      span,
    };

    let handler_case =
      HandlerCase::new(operation.clone(), pattern.clone(), expression.clone(), span);

    assert_eq!(handler_case.to_string(), "Get x => 42");
  }

  #[test]
  fn test_statement_all_variants() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let id = Identifier::new("x".to_string(), span);

    let expr_stmt = Statement::Expression {
      type_annotation: None,
      expression: Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 42, span },
        span,
      },
      span,
    };
    let let_stmt = Statement::Let {
      type_annotation: None,
      binding: Binding::new(
        Pattern::Variable {
          identifier: id.clone(),
          span,
        },
        Expression::Literal {
          type_annotation: None,
          literal: Literal::Integer { value: 42, span },
          span,
        },
        None,
        span,
      ),
      span,
    };
    let type_stmt = Statement::Type {
      type_annotation: None,
      name: id.clone(),
      parameters: vec![],
      variants: vec![],
      span,
    };
    let effect_stmt = Statement::Effect {
      type_annotation: None,
      name: id.clone(),
      operations: vec![],
      span,
    };
    let function_stmt = Statement::Function {
      type_annotation: None,
      name: id.clone(),
      parameters: vec![],
      return_type: None,
      body: Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 42, span },
        span,
      },
      span,
    };

    assert_eq!(expr_stmt.span(), span);
    assert_eq!(let_stmt.span(), span);
    assert_eq!(type_stmt.span(), span);
    assert_eq!(effect_stmt.span(), span);
    assert_eq!(function_stmt.span(), span);
  }

  #[test]
  fn test_statement_display_all_variants() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let id = Identifier::new("x".to_string(), span);

    let expr_stmt = Statement::Expression {
      type_annotation: None,
      expression: Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 42, span },
        span,
      },
      span,
    };
    let let_stmt = Statement::Let {
      type_annotation: None,
      binding: Binding::new(
        Pattern::Variable {
          identifier: id.clone(),
          span,
        },
        Expression::Literal {
          type_annotation: None,
          literal: Literal::Integer { value: 42, span },
          span,
        },
        None,
        span,
      ),
      span,
    };
    let type_stmt = Statement::Type {
      type_annotation: None,
      name: id.clone(),
      parameters: vec![],
      variants: vec![],
      span,
    };
    let effect_stmt = Statement::Effect {
      type_annotation: None,
      name: id.clone(),
      operations: vec![],
      span,
    };
    let function_stmt = Statement::Function {
      type_annotation: None,
      name: id.clone(),
      parameters: vec![],
      return_type: None,
      body: Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 42, span },
        span,
      },
      span,
    };

    assert_eq!(expr_stmt.to_string(), "42");
    assert_eq!(let_stmt.to_string(), "let x = 42");
    assert_eq!(type_stmt.to_string(), "type x = ");
    assert_eq!(effect_stmt.to_string(), "effect x {\n  \n}");
    assert_eq!(function_stmt.to_string(), "fn x() = 42");
  }

  #[test]
  fn test_statement_display_with_parameters() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let id = Identifier::new("x".to_string(), span);
    let param_id = Identifier::new("y".to_string(), span);

    let type_stmt = Statement::Type {
      type_annotation: None,
      name: id.clone(),
      parameters: vec![param_id.clone()],
      variants: vec![],
      span,
    };
    let function_stmt = Statement::Function {
      type_annotation: None,
      name: id.clone(),
      parameters: vec![FunctionParameter::new(
        Pattern::Variable {
          identifier: param_id.clone(),
          span,
        },
        None,
        span,
      )],
      return_type: Some(TypeExpr::Constructor {
        name: id.clone(),
        span,
      }),
      body: Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 42, span },
        span,
      },
      span,
    };

    assert_eq!(type_stmt.to_string(), "type x<y> = ");
    assert_eq!(function_stmt.to_string(), "fn x(y) -> x = 42");
  }

  #[test]
  fn test_type_variant_creation() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let name = Identifier::new("Some".to_string(), span);
    let fields = vec![TypeExpr::Constructor {
      name: Identifier::new("Int".to_string(), span),
      span,
    }];

    let variant = TypeVariant::new(name.clone(), fields.clone(), span);

    assert_eq!(variant.name, name);
    assert_eq!(variant.fields, fields);
    assert_eq!(variant.span, span);
  }

  #[test]
  fn test_type_variant_display() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let name = Identifier::new("Some".to_string(), span);

    let unit_variant = TypeVariant::new(name.clone(), vec![], span);
    let field_variant = TypeVariant::new(
      name.clone(),
      vec![
        TypeExpr::Constructor {
          name: Identifier::new("Int".to_string(), span),
          span,
        },
        TypeExpr::Constructor {
          name: Identifier::new("String".to_string(), span),
          span,
        },
      ],
      span,
    );

    assert_eq!(unit_variant.to_string(), "Some");
    assert_eq!(field_variant.to_string(), "Some(Int, String)");
  }

  #[test]
  fn test_effect_operation_creation() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let name = Identifier::new("Get".to_string(), span);
    let input_type = TypeExpr::Constructor {
      name: Identifier::new("Unit".to_string(), span),
      span,
    };
    let output_type = TypeExpr::Constructor {
      name: Identifier::new("Int".to_string(), span),
      span,
    };

    let operation =
      EffectOperation::new(name.clone(), input_type.clone(), output_type.clone(), span);

    assert_eq!(operation.name, name);
    assert_eq!(operation.input_type, input_type);
    assert_eq!(operation.output_type, output_type);
    assert_eq!(operation.span, span);
  }

  #[test]
  fn test_effect_operation_display() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let name = Identifier::new("Get".to_string(), span);
    let input_type = TypeExpr::Constructor {
      name: Identifier::new("Unit".to_string(), span),
      span,
    };
    let output_type = TypeExpr::Constructor {
      name: Identifier::new("Int".to_string(), span),
      span,
    };

    let operation =
      EffectOperation::new(name.clone(), input_type.clone(), output_type.clone(), span);

    assert_eq!(operation.to_string(), "Get: Unit -> Int");
  }

  #[test]
  fn test_function_parameter_creation() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let pattern = Pattern::Variable {
      identifier: Identifier::new("x".to_string(), span),
      span,
    };
    let type_annotation = Some(TypeExpr::Constructor {
      name: Identifier::new("Int".to_string(), span),
      span,
    });

    let param = FunctionParameter::new(pattern.clone(), type_annotation.clone(), span);

    assert_eq!(param.pattern, pattern);
    assert_eq!(param.type_annotation, type_annotation);
    assert_eq!(param.span, span);
  }

  #[test]
  fn test_function_parameter_display() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let pattern = Pattern::Variable {
      identifier: Identifier::new("x".to_string(), span),
      span,
    };

    let param_without_type = FunctionParameter::new(pattern.clone(), None, span);
    let param_with_type = FunctionParameter::new(
      pattern.clone(),
      Some(TypeExpr::Constructor {
        name: Identifier::new("Int".to_string(), span),
        span,
      }),
      span,
    );

    assert_eq!(param_without_type.to_string(), "x");
    assert_eq!(param_with_type.to_string(), "x: Int");
  }

  #[test]
  fn test_declaration_all_variants() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let id = Identifier::new("x".to_string(), span);

    let type_decl = Declaration::Type {
      name: id.clone(),
      parameters: vec![],
      variants: vec![],
      span,
    };
    let effect_decl = Declaration::Effect {
      name: id.clone(),
      operations: vec![],
      span,
    };
    let function_decl = Declaration::Function {
      name: id.clone(),
      parameters: vec![],
      return_type: None,
      body: Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 42, span },
        span,
      },
      span,
    };

    assert_eq!(type_decl.span(), span);
    assert_eq!(effect_decl.span(), span);
    assert_eq!(function_decl.span(), span);
  }

  #[test]
  fn test_declaration_display_all_variants() {
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
    let id = Identifier::new("x".to_string(), span);

    let type_decl = Declaration::Type {
      name: id.clone(),
      parameters: vec![],
      variants: vec![],
      span,
    };
    let effect_decl = Declaration::Effect {
      name: id.clone(),
      operations: vec![],
      span,
    };
    let function_decl = Declaration::Function {
      name: id.clone(),
      parameters: vec![],
      return_type: None,
      body: Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 42, span },
        span,
      },
      span,
    };

    assert_eq!(type_decl.to_string(), "type x = ");
    assert_eq!(effect_decl.to_string(), "effect x {\n  \n}");
    assert_eq!(function_decl.to_string(), "fn x() = 42");
  }

  #[test]
  fn test_ast_node_empty() {
    let ast = AstNode::empty();

    assert_eq!(ast.statements.len(), 0);
    assert_eq!(
      ast.span(),
      Span::new(SourceLocation::start(), SourceLocation::start())
    );
  }

  #[test]
  fn test_ast_node_add_statement_span_update() {
    let mut ast = AstNode::empty();
    let span1 = Span::new(SourceLocation::new(1, 1, 0), SourceLocation::new(1, 5, 4));
    let span2 = Span::new(SourceLocation::new(2, 1, 5), SourceLocation::new(2, 10, 14));

    let stmt1 = Statement::Expression {
      type_annotation: None,
      expression: Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer {
          value: 42,
          span: span1,
        },
        span: span1,
      },
      span: span1,
    };
    let stmt2 = Statement::Expression {
      type_annotation: None,
      expression: Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer {
          value: 100,
          span: span2,
        },
        span: span2,
      },
      span: span2,
    };

    ast.add_statement(stmt1);
    ast.add_statement(stmt2);

    assert_eq!(ast.statements.len(), 2);
    assert_eq!(ast.span().start, SourceLocation::new(1, 1, 0));
    assert_eq!(ast.span().end, SourceLocation::new(2, 10, 14));
  }

  #[test]
  fn test_ast_node_display() {
    let mut ast = AstNode::empty();
    let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));

    let stmt1 = Statement::Expression {
      type_annotation: None,
      expression: Expression::Literal {
        type_annotation: None,
        literal: Literal::Integer { value: 42, span },
        span,
      },
      span,
    };
    let stmt2 = Statement::Let {
      type_annotation: None,
      binding: Binding::new(
        Pattern::Variable {
          identifier: Identifier::new("x".to_string(), span),
          span,
        },
        Expression::Literal {
          type_annotation: None,
          literal: Literal::Integer { value: 100, span },
          span,
        },
        None,
        span,
      ),
      span,
    };

    ast.add_statement(stmt1);
    ast.add_statement(stmt2);

    let expected = "42\nlet x = 100";
    assert_eq!(ast.to_string(), expected);
  }

  #[test]
  fn test_ast_visitor_trait() {
    // Test that the trait can be implemented
    struct TestVisitor;

    impl crate::parser::visitor::Visitor for TestVisitor {
      fn visit_ast(&mut self, _ast: &crate::parser::ast::AstNode) {}
      fn visit_statement(&mut self, _statement: &crate::parser::ast::Statement) {}
      fn visit_expression(&mut self, _expression: &crate::parser::ast::Expression) {}
      fn visit_pattern(&mut self, _pattern: &crate::parser::ast::Pattern) {}
      fn visit_type_expr(&mut self, _type_expr: &crate::parser::ast::TypeExpr) {}
      fn visit_declaration(&mut self, _declaration: &crate::parser::ast::Declaration) {}
    }

    // Test that the default implementation works
    let mut visitor = TestVisitor;
    let ast = AstNode::empty();
    visitor.visit_ast(&ast);

    // This test ensures the trait can be implemented and used
    assert!(true);
  }
}
