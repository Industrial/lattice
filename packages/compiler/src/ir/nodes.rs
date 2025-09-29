//! IR node definitions for the Lattice language.
//!
//! This module provides the core data structures for the Intermediate Representation
//! in A-normal form, including expressions, statements, and control flow constructs.

use crate::lexer::SourceLocation;
use crate::types::types::Type;
use std::fmt;

/// Represents a span in the source code for IR nodes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IRSpan {
  /// Start location
  pub start: SourceLocation,
  /// End location
  pub end: SourceLocation,
}

impl IRSpan {
  /// Create a new IR span from start to end locations
  pub fn new(start: SourceLocation, end: SourceLocation) -> Self {
    Self { start, end }
  }

  /// Create an IR span from source locations
  pub fn from_locations(start: SourceLocation, end: SourceLocation) -> Self {
    Self { start, end }
  }

  /// Create a span that encompasses multiple IR spans
  pub fn from_spans(spans: &[IRSpan]) -> Option<Self> {
    if spans.is_empty() {
      return None;
    }

    let start = spans.iter().map(|s| s.start).min().unwrap();
    let end = spans.iter().map(|s| s.end).max().unwrap();
    Some(Self { start, end })
  }
}

impl fmt::Display for IRSpan {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}:{}", self.start, self.end)
  }
}

/// Represents a variable in the IR
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IRVariable {
  /// The name of the variable
  pub name: String,
  /// The source location span
  pub span: IRSpan,
  /// The type of the variable
  pub type_annotation: Option<Type>,
}

impl IRVariable {
  /// Create a new IR variable
  pub fn new(name: String, span: IRSpan) -> Self {
    Self {
      name,
      span,
      type_annotation: None,
    }
  }

  /// Create an IR variable from an AST identifier
  pub fn from_ast_identifier(name: String, span: IRSpan) -> Self {
    Self {
      name,
      span,
      type_annotation: None,
    }
  }

  /// Set the type annotation for this variable
  pub fn with_type(mut self, type_annotation: Type) -> Self {
    self.type_annotation = Some(type_annotation);
    self
  }
}

impl fmt::Display for IRVariable {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.name)
  }
}

/// Represents a literal value in the IR
#[derive(Debug, Clone, PartialEq)]
pub enum IRLiteral {
  /// Integer literal
  Integer { value: i64, span: IRSpan },
  /// Floating-point literal
  Float { value: f64, span: IRSpan },
  /// String literal
  String { value: String, span: IRSpan },
  /// Character literal
  Char { value: char, span: IRSpan },
  /// Boolean literal
  Boolean { value: bool, span: IRSpan },
  /// Unit literal (empty tuple)
  Unit { span: IRSpan },
}

impl IRLiteral {
  /// Get the span of the literal
  pub fn span(&self) -> IRSpan {
    match self {
      IRLiteral::Integer { span, .. } => *span,
      IRLiteral::Float { span, .. } => *span,
      IRLiteral::String { span, .. } => *span,
      IRLiteral::Char { span, .. } => *span,
      IRLiteral::Boolean { span, .. } => *span,
      IRLiteral::Unit { span } => *span,
    }
  }
}

impl fmt::Display for IRLiteral {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      IRLiteral::Integer { value, .. } => write!(f, "{}", value),
      IRLiteral::Float { value, .. } => write!(f, "{}", value),
      IRLiteral::String { value, .. } => write!(f, "\"{}\"", value),
      IRLiteral::Char { value, .. } => write!(f, "'{}'", value),
      IRLiteral::Boolean { value, .. } => write!(f, "{}", value),
      IRLiteral::Unit { .. } => write!(f, "()"),
    }
  }
}

/// Represents a primitive operation in the IR
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IRPrimitiveOp {
  /// Arithmetic operations
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Pow,
  /// Comparison operations
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,
  /// Logical operations
  And,
  Or,
  Not,
  /// Bitwise operations
  BitwiseAnd,
  BitwiseOr,
  BitwiseXor,
  BitwiseNot,
  LeftShift,
  RightShift,
}

impl fmt::Display for IRPrimitiveOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      IRPrimitiveOp::Add => write!(f, "+"),
      IRPrimitiveOp::Sub => write!(f, "-"),
      IRPrimitiveOp::Mul => write!(f, "*"),
      IRPrimitiveOp::Div => write!(f, "/"),
      IRPrimitiveOp::Mod => write!(f, "%"),
      IRPrimitiveOp::Pow => write!(f, "**"),
      IRPrimitiveOp::Eq => write!(f, "=="),
      IRPrimitiveOp::Ne => write!(f, "!="),
      IRPrimitiveOp::Lt => write!(f, "<"),
      IRPrimitiveOp::Le => write!(f, "<="),
      IRPrimitiveOp::Gt => write!(f, ">"),
      IRPrimitiveOp::Ge => write!(f, ">="),
      IRPrimitiveOp::And => write!(f, "&&"),
      IRPrimitiveOp::Or => write!(f, "||"),
      IRPrimitiveOp::Not => write!(f, "!"),
      IRPrimitiveOp::BitwiseAnd => write!(f, "&"),
      IRPrimitiveOp::BitwiseOr => write!(f, "|"),
      IRPrimitiveOp::BitwiseXor => write!(f, "^"),
      IRPrimitiveOp::BitwiseNot => write!(f, "~"),
      IRPrimitiveOp::LeftShift => write!(f, "<<"),
      IRPrimitiveOp::RightShift => write!(f, ">>"),
    }
  }
}

/// Represents an atomic expression in the IR (A-normal form)
#[derive(Debug, Clone, PartialEq)]
pub enum IRAtom {
  /// Variable reference
  Variable(IRVariable),
  /// Literal value
  Literal(IRLiteral),
}

impl IRAtom {
  /// Get the span of the atom
  pub fn span(&self) -> IRSpan {
    match self {
      IRAtom::Variable(var) => var.span,
      IRAtom::Literal(lit) => lit.span(),
    }
  }

  /// Get the type annotation if available
  pub fn type_annotation(&self) -> Option<&Type> {
    match self {
      IRAtom::Variable(var) => var.type_annotation.as_ref(),
      IRAtom::Literal(_) => None, // Literals don't have explicit type annotations
    }
  }
}

impl fmt::Display for IRAtom {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      IRAtom::Variable(var) => write!(f, "{}", var),
      IRAtom::Literal(lit) => write!(f, "{}", lit),
    }
  }
}

/// Represents a complex expression in the IR (A-normal form)
#[derive(Debug, Clone, PartialEq)]
pub enum IRComplexExpr {
  /// Primitive operation
  PrimitiveOp {
    operator: IRPrimitiveOp,
    operands: Vec<IRAtom>,
    span: IRSpan,
    type_annotation: Option<Type>,
  },
  /// Function application
  Application {
    function: IRAtom,
    arguments: Vec<IRAtom>,
    span: IRSpan,
    type_annotation: Option<Type>,
  },
  /// Constructor application
  Constructor {
    constructor: String,
    arguments: Vec<IRAtom>,
    span: IRSpan,
    type_annotation: Option<Type>,
  },
  /// Tuple construction
  Tuple {
    elements: Vec<IRAtom>,
    span: IRSpan,
    type_annotation: Option<Type>,
  },
  /// List construction
  List {
    elements: Vec<IRAtom>,
    span: IRSpan,
    type_annotation: Option<Type>,
  },
  /// Effect operation
  EffectOp {
    operation: String,
    arguments: Vec<IRAtom>,
    span: IRSpan,
    type_annotation: Option<Type>,
  },
}

impl IRComplexExpr {
  /// Get the span of the complex expression
  pub fn span(&self) -> IRSpan {
    match self {
      IRComplexExpr::PrimitiveOp { span, .. } => *span,
      IRComplexExpr::Application { span, .. } => *span,
      IRComplexExpr::Constructor { span, .. } => *span,
      IRComplexExpr::Tuple { span, .. } => *span,
      IRComplexExpr::List { span, .. } => *span,
      IRComplexExpr::EffectOp { span, .. } => *span,
    }
  }

  /// Get the type annotation if available
  pub fn type_annotation(&self) -> Option<&Type> {
    match self {
      IRComplexExpr::PrimitiveOp {
        type_annotation, ..
      } => type_annotation.as_ref(),
      IRComplexExpr::Application {
        type_annotation, ..
      } => type_annotation.as_ref(),
      IRComplexExpr::Constructor {
        type_annotation, ..
      } => type_annotation.as_ref(),
      IRComplexExpr::Tuple {
        type_annotation, ..
      } => type_annotation.as_ref(),
      IRComplexExpr::List {
        type_annotation, ..
      } => type_annotation.as_ref(),
      IRComplexExpr::EffectOp {
        type_annotation, ..
      } => type_annotation.as_ref(),
    }
  }
}

impl fmt::Display for IRComplexExpr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      IRComplexExpr::PrimitiveOp {
        operator, operands, ..
      } => match operands.len() {
        1 => write!(f, "{}{}", operator, operands[0]),
        2 => write!(f, "{} {} {}", operands[0], operator, operands[1]),
        _ => {
          let ops = operands
            .iter()
            .map(|o| o.to_string())
            .collect::<Vec<_>>()
            .join(" ");
          write!(f, "{} {}", ops, operator)
        }
      },
      IRComplexExpr::Application {
        function,
        arguments,
        ..
      } => {
        if arguments.is_empty() {
          write!(f, "{}", function)
        } else {
          let args = arguments
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<_>>()
            .join(", ");
          write!(f, "{}({})", function, args)
        }
      }
      IRComplexExpr::Constructor {
        constructor,
        arguments,
        ..
      } => {
        if arguments.is_empty() {
          write!(f, "{}", constructor)
        } else {
          let args = arguments
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<_>>()
            .join(", ");
          write!(f, "{}({})", constructor, args)
        }
      }
      IRComplexExpr::Tuple { elements, .. } => {
        let elems = elements
          .iter()
          .map(|e| e.to_string())
          .collect::<Vec<_>>()
          .join(", ");
        write!(f, "({})", elems)
      }
      IRComplexExpr::List { elements, .. } => {
        let elems = elements
          .iter()
          .map(|e| e.to_string())
          .collect::<Vec<_>>()
          .join(", ");
        write!(f, "[{}]", elems)
      }
      IRComplexExpr::EffectOp {
        operation,
        arguments,
        ..
      } => {
        if arguments.is_empty() {
          write!(f, "do {}", operation)
        } else {
          let args = arguments
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<_>>()
            .join(", ");
          write!(f, "do {}({})", operation, args)
        }
      }
    }
  }
}

/// Represents a statement in the IR
#[derive(Debug, Clone, PartialEq)]
pub enum IRStatement {
  /// Let binding: let var = expr
  Let {
    variable: IRVariable,
    expression: IRComplexExpr,
    span: IRSpan,
  },
  /// Assignment: var = expr
  Assignment {
    variable: IRVariable,
    expression: IRComplexExpr,
    span: IRSpan,
  },
  /// Expression statement (for side effects)
  Expression {
    expression: IRComplexExpr,
    span: IRSpan,
  },
  /// Return statement
  Return {
    expression: Option<IRAtom>,
    span: IRSpan,
  },
}

impl IRStatement {
  /// Get the span of the statement
  pub fn span(&self) -> IRSpan {
    match self {
      IRStatement::Let { span, .. } => *span,
      IRStatement::Assignment { span, .. } => *span,
      IRStatement::Expression { span, .. } => *span,
      IRStatement::Return { span, .. } => *span,
    }
  }
}

impl fmt::Display for IRStatement {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      IRStatement::Let {
        variable,
        expression,
        ..
      } => {
        write!(f, "let {} = {}", variable, expression)
      }
      IRStatement::Assignment {
        variable,
        expression,
        ..
      } => {
        write!(f, "{} = {}", variable, expression)
      }
      IRStatement::Expression { expression, .. } => {
        write!(f, "{}", expression)
      }
      IRStatement::Return { expression, .. } => match expression {
        Some(expr) => write!(f, "return {}", expr),
        None => write!(f, "return"),
      },
    }
  }
}

/// Represents a control flow construct in the IR
#[derive(Debug, Clone, PartialEq)]
pub enum IRControlFlow {
  /// If-then-else: if condition then then_branch else else_branch
  If {
    condition: IRAtom,
    then_branch: Vec<IRStatement>,
    else_branch: Vec<IRStatement>,
    span: IRSpan,
  },
  /// Match expression: match scrutinee with arms
  Match {
    scrutinee: IRAtom,
    arms: Vec<IRMatchArm>,
    span: IRSpan,
  },
  /// Loop: loop body
  Loop {
    body: Vec<IRStatement>,
    span: IRSpan,
  },
  /// Break from loop
  Break { span: IRSpan },
  /// Continue in loop
  Continue { span: IRSpan },
}

impl IRControlFlow {
  /// Get the span of the control flow construct
  pub fn span(&self) -> IRSpan {
    match self {
      IRControlFlow::If { span, .. } => *span,
      IRControlFlow::Match { span, .. } => *span,
      IRControlFlow::Loop { span, .. } => *span,
      IRControlFlow::Break { span } => *span,
      IRControlFlow::Continue { span } => *span,
    }
  }
}

impl fmt::Display for IRControlFlow {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      IRControlFlow::If {
        condition,
        then_branch,
        else_branch,
        ..
      } => {
        let then_str = then_branch
          .iter()
          .map(|s| s.to_string())
          .collect::<Vec<_>>()
          .join("; ");
        let else_str = else_branch
          .iter()
          .map(|s| s.to_string())
          .collect::<Vec<_>>()
          .join("; ");
        write!(
          f,
          "if {} then {{ {} }} else {{ {} }}",
          condition, then_str, else_str
        )
      }
      IRControlFlow::Match {
        scrutinee, arms, ..
      } => {
        let arms_str = arms
          .iter()
          .map(|a| a.to_string())
          .collect::<Vec<_>>()
          .join(" | ");
        write!(f, "match {} with {}", scrutinee, arms_str)
      }
      IRControlFlow::Loop { body, .. } => {
        let body_str = body
          .iter()
          .map(|s| s.to_string())
          .collect::<Vec<_>>()
          .join("; ");
        write!(f, "loop {{ {} }}", body_str)
      }
      IRControlFlow::Break { .. } => write!(f, "break"),
      IRControlFlow::Continue { .. } => write!(f, "continue"),
    }
  }
}

/// Represents a match arm in the IR
#[derive(Debug, Clone, PartialEq)]
pub struct IRMatchArm {
  /// The pattern to match
  pub pattern: IRPattern,
  /// The expression to evaluate if the pattern matches
  pub expression: IRAtom,
  /// Optional guard condition
  pub guard: Option<IRAtom>,
  /// The source location span
  pub span: IRSpan,
}

impl IRMatchArm {
  /// Create a new match arm
  pub fn new(pattern: IRPattern, expression: IRAtom, guard: Option<IRAtom>, span: IRSpan) -> Self {
    Self {
      pattern,
      expression,
      guard,
      span,
    }
  }
}

impl fmt::Display for IRMatchArm {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.guard {
      Some(guard) => write!(f, "{} when {} => {}", self.pattern, guard, self.expression),
      None => write!(f, "{} => {}", self.pattern, self.expression),
    }
  }
}

/// Represents a pattern in the IR (desugared from AST patterns)
#[derive(Debug, Clone, PartialEq)]
pub enum IRPattern {
  /// Variable pattern (e.g., `x`)
  Variable { variable: IRVariable },
  /// Wildcard pattern (e.g., `_`)
  Wildcard { span: IRSpan },
  /// Literal pattern (e.g., `42`, `"hello"`)
  Literal { literal: IRLiteral },
  /// Constructor pattern (e.g., `Some(x)`, `Cons(head, tail)`)
  Constructor {
    constructor: String,
    arguments: Vec<IRPattern>,
    span: IRSpan,
  },
  /// Tuple pattern (e.g., `(x, y)`)
  Tuple {
    elements: Vec<IRPattern>,
    span: IRSpan,
  },
  /// List pattern (e.g., `[x, y, z]`)
  List {
    elements: Vec<IRPattern>,
    span: IRSpan,
  },
}

impl IRPattern {
  /// Get the span of the pattern
  pub fn span(&self) -> IRSpan {
    match self {
      IRPattern::Variable { variable } => variable.span,
      IRPattern::Wildcard { span } => *span,
      IRPattern::Literal { literal } => literal.span(),
      IRPattern::Constructor { span, .. } => *span,
      IRPattern::Tuple { span, .. } => *span,
      IRPattern::List { span, .. } => *span,
    }
  }
}

impl fmt::Display for IRPattern {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      IRPattern::Variable { variable } => write!(f, "{}", variable),
      IRPattern::Wildcard { .. } => write!(f, "_"),
      IRPattern::Literal { literal } => write!(f, "{}", literal),
      IRPattern::Constructor {
        constructor,
        arguments,
        ..
      } => {
        if arguments.is_empty() {
          write!(f, "{}", constructor)
        } else {
          let args = arguments
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<_>>()
            .join(", ");
          write!(f, "{}({})", constructor, args)
        }
      }
      IRPattern::Tuple { elements, .. } => {
        let elems = elements
          .iter()
          .map(|p| p.to_string())
          .collect::<Vec<_>>()
          .join(", ");
        write!(f, "({})", elems)
      }
      IRPattern::List { elements, .. } => {
        let elems = elements
          .iter()
          .map(|p| p.to_string())
          .collect::<Vec<_>>()
          .join(", ");
        write!(f, "[{}]", elems)
      }
    }
  }
}

/// Represents a function definition in the IR
#[derive(Debug, Clone, PartialEq)]
pub struct IRFunction {
  /// The name of the function
  pub name: String,
  /// The parameters of the function
  pub parameters: Vec<IRVariable>,
  /// The return type
  pub return_type: Option<Type>,
  /// The body of the function
  pub body: Vec<IRStatement>,
  /// The source location span
  pub span: IRSpan,
  /// Whether this function is exported
  pub exported: bool,
}

impl IRFunction {
  /// Create a new function definition
  pub fn new(
    name: String,
    parameters: Vec<IRVariable>,
    return_type: Option<Type>,
    body: Vec<IRStatement>,
    span: IRSpan,
  ) -> Self {
    Self {
      name,
      parameters,
      return_type,
      body,
      span,
      exported: false,
    }
  }

  /// Set the function as exported
  pub fn exported(mut self) -> Self {
    self.exported = true;
    self
  }
}

impl fmt::Display for IRFunction {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let params = self
      .parameters
      .iter()
      .map(|p| p.to_string())
      .collect::<Vec<_>>()
      .join(", ");
    let return_str = self
      .return_type
      .as_ref()
      .map(|t| format!(" -> {}", t))
      .unwrap_or_default();
    let body_str = self
      .body
      .iter()
      .map(|s| s.to_string())
      .collect::<Vec<_>>()
      .join("; ");
    let export_str = if self.exported { "export " } else { "" };
    write!(
      f,
      "{}fn {}({}){} = {{ {} }}",
      export_str, self.name, params, return_str, body_str
    )
  }
}

/// Represents a type definition in the IR
#[derive(Debug, Clone, PartialEq)]
pub struct IRTypeDef {
  /// The name of the type
  pub name: String,
  /// The type parameters
  pub parameters: Vec<String>,
  /// The variants of the type
  pub variants: Vec<IRTypeVariant>,
  /// The source location span
  pub span: IRSpan,
}

impl IRTypeDef {
  /// Create a new type definition
  pub fn new(
    name: String,
    parameters: Vec<String>,
    variants: Vec<IRTypeVariant>,
    span: IRSpan,
  ) -> Self {
    Self {
      name,
      parameters,
      variants,
      span,
    }
  }
}

impl fmt::Display for IRTypeDef {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let params_str = if self.parameters.is_empty() {
      String::new()
    } else {
      format!("<{}>", self.parameters.join(", "))
    };
    let variants_str = self
      .variants
      .iter()
      .map(|v| v.to_string())
      .collect::<Vec<_>>()
      .join(" | ");
    write!(f, "type {}{} = {}", self.name, params_str, variants_str)
  }
}

/// Represents a type variant in the IR
#[derive(Debug, Clone, PartialEq)]
pub struct IRTypeVariant {
  /// The name of the variant
  pub name: String,
  /// The fields of the variant
  pub fields: Vec<Type>,
  /// The source location span
  pub span: IRSpan,
}

impl IRTypeVariant {
  /// Create a new type variant
  pub fn new(name: String, fields: Vec<Type>, span: IRSpan) -> Self {
    Self { name, fields, span }
  }
}

impl fmt::Display for IRTypeVariant {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.fields.is_empty() {
      write!(f, "{}", self.name)
    } else {
      let fields_str = self
        .fields
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(", ");
      write!(f, "{}({})", self.name, fields_str)
    }
  }
}

/// Represents an effect definition in the IR
#[derive(Debug, Clone, PartialEq)]
pub struct IREffectDef {
  /// The name of the effect
  pub name: String,
  /// The operations of the effect
  pub operations: Vec<IREffectOperation>,
  /// The source location span
  pub span: IRSpan,
}

impl IREffectDef {
  /// Create a new effect definition
  pub fn new(name: String, operations: Vec<IREffectOperation>, span: IRSpan) -> Self {
    Self {
      name,
      operations,
      span,
    }
  }
}

impl fmt::Display for IREffectDef {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let ops_str = self
      .operations
      .iter()
      .map(|o| o.to_string())
      .collect::<Vec<_>>()
      .join("; ");
    write!(f, "effect {} {{ {} }}", self.name, ops_str)
  }
}

/// Represents an effect operation in the IR
#[derive(Debug, Clone, PartialEq)]
pub struct IREffectOperation {
  /// The name of the operation
  pub name: String,
  /// The input type of the operation
  pub input_type: Type,
  /// The output type of the operation
  pub output_type: Type,
  /// The source location span
  pub span: IRSpan,
}

impl IREffectOperation {
  /// Create a new effect operation
  pub fn new(name: String, input_type: Type, output_type: Type, span: IRSpan) -> Self {
    Self {
      name,
      input_type,
      output_type,
      span,
    }
  }
}

impl fmt::Display for IREffectOperation {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}: {} -> {}",
      self.name, self.input_type, self.output_type
    )
  }
}

/// Represents the root node of the IR
#[derive(Debug, Clone, PartialEq)]
pub struct IRNode {
  /// The functions defined in the program
  pub functions: Vec<IRFunction>,
  /// The type definitions
  pub type_definitions: Vec<IRTypeDef>,
  /// The effect definitions
  pub effect_definitions: Vec<IREffectDef>,
  /// The source location span
  pub span: IRSpan,
}

impl IRNode {
  /// Create a new IR node
  pub fn new(
    functions: Vec<IRFunction>,
    type_definitions: Vec<IRTypeDef>,
    effect_definitions: Vec<IREffectDef>,
    span: IRSpan,
  ) -> Self {
    Self {
      functions,
      type_definitions,
      effect_definitions,
      span,
    }
  }

  /// Create an empty IR node
  pub fn empty() -> Self {
    Self {
      functions: Vec::new(),
      type_definitions: Vec::new(),
      effect_definitions: Vec::new(),
      span: IRSpan::new(SourceLocation::start(), SourceLocation::start()),
    }
  }

  /// Add a function to the IR
  pub fn add_function(&mut self, function: IRFunction) {
    let function_span = function.span;
    self.functions.push(function);
    // Update the span to include the new function
    if let Some(span) = IRSpan::from_spans(&[self.span, function_span]) {
      self.span = span;
    }
  }

  /// Add a type definition to the IR
  pub fn add_type_definition(&mut self, type_def: IRTypeDef) {
    let type_span = type_def.span;
    self.type_definitions.push(type_def);
    // Update the span to include the new type definition
    if let Some(span) = IRSpan::from_spans(&[self.span, type_span]) {
      self.span = span;
    }
  }

  /// Add an effect definition to the IR
  pub fn add_effect_definition(&mut self, effect_def: IREffectDef) {
    let effect_span = effect_def.span;
    self.effect_definitions.push(effect_def);
    // Update the span to include the new effect definition
    if let Some(span) = IRSpan::from_spans(&[self.span, effect_span]) {
      self.span = span;
    }
  }

  /// Get the span of the IR
  pub fn span(&self) -> IRSpan {
    self.span
  }
}

impl fmt::Display for IRNode {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // Print type definitions first
    for type_def in &self.type_definitions {
      writeln!(f, "{}", type_def)?;
    }

    // Print effect definitions
    for effect_def in &self.effect_definitions {
      writeln!(f, "{}", effect_def)?;
    }

    // Print functions
    for function in &self.functions {
      writeln!(f, "{}", function)?;
    }

    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::{SourceLocation, TokenKind};

  fn test_span() -> IRSpan {
    IRSpan::new(SourceLocation::start(), SourceLocation::new(1, 5, 4))
  }

  #[test]
  fn test_ir_variable_creation() {
    let span = test_span();
    let var = IRVariable::new("x".to_string(), span);

    assert_eq!(var.name, "x");
    assert_eq!(var.span, span);
    assert_eq!(var.type_annotation, None);
  }

  #[test]
  fn test_ir_variable_with_type() {
    let span = test_span();
    let var = IRVariable::new("x".to_string(), span)
      .with_type(Type::primitive(crate::types::types::PrimitiveType::Int));

    assert_eq!(var.name, "x");
    assert!(var.type_annotation.is_some());
  }

  #[test]
  fn test_ir_literal_all_variants() {
    let span = test_span();

    let int_lit = IRLiteral::Integer { value: 42, span };
    let float_lit = IRLiteral::Float { value: 3.14, span };
    let str_lit = IRLiteral::String {
      value: "hello".to_string(),
      span,
    };
    let char_lit = IRLiteral::Char { value: 'a', span };
    let bool_lit = IRLiteral::Boolean { value: true, span };
    let unit_lit = IRLiteral::Unit { span };

    assert_eq!(int_lit.span(), span);
    assert_eq!(float_lit.span(), span);
    assert_eq!(str_lit.span(), span);
    assert_eq!(char_lit.span(), span);
    assert_eq!(bool_lit.span(), span);
    assert_eq!(unit_lit.span(), span);
  }

  #[test]
  fn test_ir_literal_display() {
    let span = test_span();

    let int_lit = IRLiteral::Integer { value: 42, span };
    let str_lit = IRLiteral::String {
      value: "hello".to_string(),
      span,
    };

    assert_eq!(int_lit.to_string(), "42");
    assert_eq!(str_lit.to_string(), "\"hello\"");
  }

  #[test]
  fn test_ir_atom_creation() {
    let span = test_span();
    let var = IRVariable::new("x".to_string(), span);
    let lit = IRLiteral::Integer { value: 42, span };

    let var_atom = IRAtom::Variable(var.clone());
    let lit_atom = IRAtom::Literal(lit.clone());

    assert_eq!(var_atom.span(), span);
    assert_eq!(lit_atom.span(), span);
  }

  #[test]
  fn test_ir_complex_expr_creation() {
    let span = test_span();
    let var = IRAtom::Variable(IRVariable::new("x".to_string(), span));
    let lit = IRAtom::Literal(IRLiteral::Integer { value: 1, span });

    let prim_op = IRComplexExpr::PrimitiveOp {
      operator: IRPrimitiveOp::Add,
      operands: vec![var.clone(), lit.clone()],
      span,
      type_annotation: Some(Type::primitive(crate::types::types::PrimitiveType::Int)),
    };

    assert_eq!(prim_op.span(), span);
    assert!(prim_op.type_annotation().is_some());
  }

  #[test]
  fn test_ir_statement_creation() {
    let span = test_span();
    let var = IRVariable::new("x".to_string(), span);
    let expr = IRComplexExpr::PrimitiveOp {
      operator: IRPrimitiveOp::Add,
      operands: vec![
        IRAtom::Variable(IRVariable::new("a".to_string(), span)),
        IRAtom::Literal(IRLiteral::Integer { value: 1, span }),
      ],
      span,
      type_annotation: None,
    };

    let let_stmt = IRStatement::Let {
      variable: var.clone(),
      expression: expr.clone(),
      span,
    };

    assert_eq!(let_stmt.span(), span);
  }

  #[test]
  fn test_ir_function_creation() {
    let span = test_span();
    let param = IRVariable::new("x".to_string(), span);
    let body = vec![IRStatement::Return {
      expression: Some(IRAtom::Variable(param.clone())),
      span,
    }];

    let function = IRFunction::new(
      "test".to_string(),
      vec![param],
      Some(Type::primitive(crate::types::types::PrimitiveType::Int)),
      body,
      span,
    );

    assert_eq!(function.name, "test");
    assert_eq!(function.parameters.len(), 1);
    assert_eq!(function.body.len(), 1);
  }

  #[test]
  fn test_ir_node_creation() {
    let span = test_span();
    let function = IRFunction::new("test".to_string(), vec![], None, vec![], span);

    let ir_node = IRNode::new(vec![function], vec![], vec![], span);

    assert_eq!(ir_node.functions.len(), 1);
    assert_eq!(ir_node.type_definitions.len(), 0);
    assert_eq!(ir_node.effect_definitions.len(), 0);
  }

  #[test]
  fn test_ir_node_add_function() {
    let mut ir_node = IRNode::empty();
    let span = test_span();
    let function = IRFunction::new("test".to_string(), vec![], None, vec![], span);

    ir_node.add_function(function);

    assert_eq!(ir_node.functions.len(), 1);
  }

  #[test]
  fn test_ir_pattern_creation() {
    let span = test_span();
    let var_pattern = IRPattern::Variable {
      variable: IRVariable::new("x".to_string(), span),
    };
    let wildcard_pattern = IRPattern::Wildcard { span };
    let literal_pattern = IRPattern::Literal {
      literal: IRLiteral::Integer { value: 42, span },
    };

    assert_eq!(var_pattern.span(), span);
    assert_eq!(wildcard_pattern.span(), span);
    assert_eq!(literal_pattern.span(), span);
  }

  #[test]
  fn test_ir_match_arm_creation() {
    let span = test_span();
    let pattern = IRPattern::Variable {
      variable: IRVariable::new("x".to_string(), span),
    };
    let expression = IRAtom::Literal(IRLiteral::Integer { value: 42, span });

    let match_arm = IRMatchArm::new(pattern.clone(), expression.clone(), None, span);

    assert_eq!(match_arm.pattern, pattern);
    assert_eq!(match_arm.expression, expression);
    assert_eq!(match_arm.guard, None);
  }

  #[test]
  fn test_ir_control_flow_creation() {
    let span = test_span();
    let condition = IRAtom::Variable(IRVariable::new("x".to_string(), span));
    let then_branch = vec![IRStatement::Return {
      expression: None,
      span,
    }];
    let else_branch = vec![IRStatement::Return {
      expression: None,
      span,
    }];

    let if_expr = IRControlFlow::If {
      condition,
      then_branch,
      else_branch,
      span,
    };

    assert_eq!(if_expr.span(), span);
  }

  #[test]
  fn test_ir_type_def_creation() {
    let span = test_span();
    let variant = IRTypeVariant::new(
      "Some".to_string(),
      vec![Type::primitive(crate::types::types::PrimitiveType::Int)],
      span,
    );

    let type_def = IRTypeDef::new(
      "Option".to_string(),
      vec!["T".to_string()],
      vec![variant],
      span,
    );

    assert_eq!(type_def.name, "Option");
    assert_eq!(type_def.parameters.len(), 1);
    assert_eq!(type_def.variants.len(), 1);
  }

  #[test]
  fn test_ir_effect_def_creation() {
    let span = test_span();
    let operation = IREffectOperation::new(
      "Get".to_string(),
      Type::primitive(crate::types::types::PrimitiveType::Unit),
      Type::primitive(crate::types::types::PrimitiveType::Int),
      span,
    );

    let effect_def = IREffectDef::new("State".to_string(), vec![operation], span);

    assert_eq!(effect_def.name, "State");
    assert_eq!(effect_def.operations.len(), 1);
  }

  #[test]
  fn test_display_implementations() {
    let span = test_span();

    // Test variable display
    let var = IRVariable::new("x".to_string(), span);
    assert_eq!(var.to_string(), "x");

    // Test literal display
    let lit = IRLiteral::Integer { value: 42, span };
    assert_eq!(lit.to_string(), "42");

    // Test atom display
    let atom = IRAtom::Variable(var);
    assert_eq!(atom.to_string(), "x");

    // Test complex expression display
    let expr = IRComplexExpr::PrimitiveOp {
      operator: IRPrimitiveOp::Add,
      operands: vec![
        IRAtom::Variable(IRVariable::new("a".to_string(), span)),
        IRAtom::Literal(IRLiteral::Integer { value: 1, span }),
      ],
      span,
      type_annotation: None,
    };
    assert_eq!(expr.to_string(), "a + 1");

    // Test statement display
    let stmt = IRStatement::Let {
      variable: IRVariable::new("x".to_string(), span),
      expression: expr,
      span,
    };
    assert_eq!(stmt.to_string(), "let x = a + 1");
  }
}
