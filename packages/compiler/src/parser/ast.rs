//! Abstract Syntax Tree (AST) definitions for the Lattice language.
//!
//! This module provides the data structures for representing the parsed structure
//! of Lattice programs, including expressions, statements, declarations, patterns,
//! and type expressions.

use crate::lexer::{SourceLocation, Token};
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
    Integer {
        value: i64,
        span: Span,
    },
    /// Floating-point literal
    Float {
        value: f64,
        span: Span,
    },
    /// String literal
    String {
        value: String,
        span: Span,
    },
    /// Character literal
    Char {
        value: char,
        span: Span,
    },
    /// Boolean literal
    Boolean {
        value: bool,
        span: Span,
    },
    /// Unit literal (empty tuple)
    Unit {
        span: Span,
    },
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
    Variable {
        identifier: Identifier,
        span: Span,
    },
    /// Wildcard pattern (e.g., `_`)
    Wildcard {
        span: Span,
    },
    /// Literal pattern (e.g., `42`, `"hello"`)
    Literal {
        literal: Literal,
        span: Span,
    },
    /// Constructor pattern (e.g., `Some(x)`, `Cons(head, tail)`)
    Constructor {
        constructor: Identifier,
        arguments: Vec<Pattern>,
        span: Span,
    },
    /// Tuple pattern (e.g., `(x, y)`)
    Tuple {
        elements: Vec<Pattern>,
        span: Span,
    },
    /// List pattern (e.g., `[x, y, z]`)
    List {
        elements: Vec<Pattern>,
        span: Span,
    },
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
            Pattern::Constructor { constructor, arguments, .. } => {
                if arguments.is_empty() {
                    write!(f, "{}", constructor)
                } else {
                    write!(f, "{}({})", constructor, 
                           arguments.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", "))
                }
            }
            Pattern::Tuple { elements, .. } => {
                write!(f, "({})", elements.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", "))
            }
            Pattern::List { elements, .. } => {
                write!(f, "[{}]", elements.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", "))
            }
            Pattern::Or { left, right, .. } => {
                write!(f, "{} | {}", left, right)
            }
            Pattern::As { pattern, identifier, .. } => {
                write!(f, "{} as {}", pattern, identifier)
            }
        }
    }
}

/// Represents a type expression in the AST
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExpr {
    /// Type variable (e.g., `'a`)
    Variable {
        name: String,
        span: Span,
    },
    /// Type constructor (e.g., `Int`, `String`)
    Constructor {
        name: Identifier,
        span: Span,
    },
    /// Function type (e.g., `Int -> String`)
    Function {
        parameter: Box<TypeExpr>,
        return_type: Box<TypeExpr>,
        span: Span,
    },
    /// Tuple type (e.g., `(Int, String)`)
    Tuple {
        elements: Vec<TypeExpr>,
        span: Span,
    },
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
            TypeExpr::Function { parameter, return_type, .. } => {
                write!(f, "{} -> {}", parameter, return_type)
            }
            TypeExpr::Tuple { elements, .. } => {
                write!(f, "({})", elements.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "))
            }
            TypeExpr::List { element_type, .. } => {
                write!(f, "[{}]", element_type)
            }
            TypeExpr::Generic { constructor, arguments, .. } => {
                write!(f, "{}<{}>", constructor, 
                       arguments.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "))
            }
            TypeExpr::Effect { input_type, output_type, .. } => {
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
    },
    /// Variable reference
    Variable {
        identifier: Identifier,
        span: Span,
    },
    /// Function application (e.g., `f(x, y)`)
    Application {
        function: Box<Expression>,
        arguments: Vec<Expression>,
        span: Span,
    },
    /// Binary operation (e.g., `x + y`)
    BinaryOp {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
        span: Span,
    },
    /// Unary operation (e.g., `!x`)
    UnaryOp {
        operator: UnaryOperator,
        operand: Box<Expression>,
        span: Span,
    },
    /// Let expression (e.g., `let x = 42 in x + 1`)
    Let {
        bindings: Vec<Binding>,
        body: Box<Expression>,
        span: Span,
    },
    /// If expression (e.g., `if x > 0 then x else -x`)
    If {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Box<Expression>,
        span: Span,
    },
    /// Match expression (e.g., `match x with | Some(y) => y | None => 0`)
    Match {
        scrutinee: Box<Expression>,
        arms: Vec<MatchArm>,
        span: Span,
    },
    /// Lambda expression (e.g., `\x -> x + 1`)
    Lambda {
        parameters: Vec<Pattern>,
        body: Box<Expression>,
        span: Span,
    },
    /// Tuple expression (e.g., `(x, y)`)
    Tuple {
        elements: Vec<Expression>,
        span: Span,
    },
    /// List expression (e.g., `[1, 2, 3]`)
    List {
        elements: Vec<Expression>,
        span: Span,
    },
    /// Effect operation (e.g., `do Get`)
    EffectOp {
        operation: Identifier,
        arguments: Vec<Expression>,
        span: Span,
    },
    /// Effect handler (e.g., `handle e with Get => resume 42`)
    Handler {
        expression: Box<Expression>,
        cases: Vec<HandlerCase>,
        span: Span,
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
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Literal { literal, .. } => write!(f, "{}", literal),
            Expression::Variable { identifier, .. } => write!(f, "{}", identifier),
            Expression::Application { function, arguments, .. } => {
                if arguments.is_empty() {
                    write!(f, "{}", function)
                } else {
                    write!(f, "{}({})", function, 
                           arguments.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(", "))
                }
            }
            Expression::BinaryOp { left, operator, right, .. } => {
                write!(f, "{} {} {}", left, operator, right)
            }
            Expression::UnaryOp { operator, operand, .. } => {
                write!(f, "{}{}", operator, operand)
            }
            Expression::Let { bindings, body, .. } => {
                write!(f, "let {} in {}", 
                       bindings.iter().map(|b| b.to_string()).collect::<Vec<_>>().join("; "), 
                       body)
            }
            Expression::If { condition, then_branch, else_branch, .. } => {
                write!(f, "if {} then {} else {}", condition, then_branch, else_branch)
            }
            Expression::Match { scrutinee, arms, .. } => {
                write!(f, "match {} with {}", scrutinee, 
                       arms.iter().map(|a| a.to_string()).collect::<Vec<_>>().join(" | "))
            }
            Expression::Lambda { parameters, body, .. } => {
                write!(f, "\\{} -> {}", 
                       parameters.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(" "), 
                       body)
            }
            Expression::Tuple { elements, .. } => {
                write!(f, "({})", elements.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(", "))
            }
            Expression::List { elements, .. } => {
                write!(f, "[{}]", elements.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(", "))
            }
            Expression::EffectOp { operation, arguments, .. } => {
                if arguments.is_empty() {
                    write!(f, "do {}", operation)
                } else {
                    write!(f, "do {}({})", operation, 
                           arguments.iter().map(|e| e.to_string()).collect::<Vec<_>>().join(", "))
                }
            }
            Expression::Handler { expression, cases, .. } => {
                write!(f, "handle {} with {}", expression, 
                       cases.iter().map(|c| c.to_string()).collect::<Vec<_>>().join(" | "))
            }
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
    pub fn new(pattern: Pattern, expression: Expression, type_annotation: Option<TypeExpr>, span: Span) -> Self {
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
    pub fn new(pattern: Pattern, expression: Expression, guard: Option<Expression>, span: Span) -> Self {
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
        write!(f, "{} {} => {}", self.operation, self.pattern, self.expression)
    }
}

/// Represents a statement in the AST
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// Expression statement
    Expression {
        expression: Expression,
        span: Span,
    },
    /// Let binding statement
    Let {
        binding: Binding,
        span: Span,
    },
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

impl Statement {
    /// Get the span of the statement
    pub fn span(&self) -> Span {
        match self {
            Statement::Expression { span, .. } => *span,
            Statement::Let { span, .. } => *span,
            Statement::Type { span, .. } => *span,
            Statement::Effect { span, .. } => *span,
            Statement::Function { span, .. } => *span,
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Expression { expression, .. } => write!(f, "{}", expression),
            Statement::Let { binding, .. } => write!(f, "let {}", binding),
            Statement::Type { name, parameters, variants, .. } => {
                if parameters.is_empty() {
                    write!(f, "type {} = {}", name, 
                           variants.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" | "))
                } else {
                    write!(f, "type {}<{}> = {}", 
                           name, 
                           parameters.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", "),
                           variants.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" | "))
                }
            }
            Statement::Effect { name, operations, .. } => {
                write!(f, "effect {} {{\n  {}\n}}", name, 
                       operations.iter().map(|o| o.to_string()).collect::<Vec<_>>().join("\n  "))
            }
            Statement::Function { name, parameters, return_type, body, .. } => {
                let param_str = parameters.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(" ");
                let return_str = return_type.as_ref().map(|t| format!(" -> {}", t)).unwrap_or_default();
                write!(f, "fn {}({}){} = {}", name, param_str, return_str, body)
            }
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
            write!(f, "{}({})", self.name, 
                   self.fields.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "))
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
        write!(f, "{}: {} -> {}", self.name, self.input_type, self.output_type)
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
            Declaration::Type { name, parameters, variants, .. } => {
                if parameters.is_empty() {
                    write!(f, "type {} = {}", name, 
                           variants.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" | "))
                } else {
                    write!(f, "type {}<{}> = {}", 
                           name, 
                           parameters.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", "),
                           variants.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" | "))
                }
            }
            Declaration::Effect { name, operations, .. } => {
                write!(f, "effect {} {{\n  {}\n}}", name, 
                       operations.iter().map(|o| o.to_string()).collect::<Vec<_>>().join("\n  "))
            }
            Declaration::Function { name, parameters, return_type, body, .. } => {
                let param_str = parameters.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(" ");
                let return_str = return_type.as_ref().map(|t| format!(" -> {}", t)).unwrap_or_default();
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
        self.statements.push(statement);
        // Update the span to include the new statement
        if let Some(span) = Span::from_spans(&[self.span, statement.span()]) {
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
    T: Visitor,
{
    fn visit_ast(&mut self, ast: &AstNode) {
        for statement in &ast.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Expression { expression, .. } => {
                self.visit_expression(expression);
            }
            Statement::Let { binding, .. } => {
                self.visit_pattern(&binding.pattern);
                self.visit_expression(&binding.expression);
                if let Some(ty) = &binding.type_annotation {
                    self.visit_type_expr(ty);
                }
            }
            Statement::Type { variants, .. } => {
                for variant in variants {
                    for field in &variant.fields {
                        self.visit_type_expr(field);
                    }
                }
            }
            Statement::Effect { operations, .. } => {
                for operation in operations {
                    self.visit_type_expr(&operation.input_type);
                    self.visit_type_expr(&operation.output_type);
                }
            }
            Statement::Function { parameters, return_type, body, .. } => {
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

    fn visit_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Application { function, arguments, .. } => {
                self.visit_expression(function);
                for arg in arguments {
                    self.visit_expression(arg);
                }
            }
            Expression::BinaryOp { left, right, .. } => {
                self.visit_expression(left);
                self.visit_expression(right);
            }
            Expression::UnaryOp { operand, .. } => {
                self.visit_expression(operand);
            }
            Expression::Let { bindings, body, .. } => {
                for binding in bindings {
                    self.visit_pattern(&binding.pattern);
                    self.visit_expression(&binding.expression);
                    if let Some(ty) = &binding.type_annotation {
                        self.visit_type_expr(ty);
                    }
                }
                self.visit_expression(body);
            }
            Expression::If { condition, then_branch, else_branch, .. } => {
                self.visit_expression(condition);
                self.visit_expression(then_branch);
                self.visit_expression(else_branch);
            }
            Expression::Match { scrutinee, arms, .. } => {
                self.visit_expression(scrutinee);
                for arm in arms {
                    self.visit_pattern(&arm.pattern);
                    self.visit_expression(&arm.expression);
                    if let Some(guard) = &arm.guard {
                        self.visit_expression(guard);
                    }
                }
            }
            Expression::Lambda { parameters, body, .. } => {
                for param in parameters {
                    self.visit_pattern(param);
                }
                self.visit_expression(body);
            }
            Expression::Tuple { elements, .. } => {
                for element in elements {
                    self.visit_expression(element);
                }
            }
            Expression::List { elements, .. } => {
                for element in elements {
                    self.visit_expression(element);
                }
            }
            Expression::EffectOp { arguments, .. } => {
                for arg in arguments {
                    self.visit_expression(arg);
                }
            }
            Expression::Handler { expression, cases, .. } => {
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
            TypeExpr::Function { parameter, return_type, .. } => {
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
            TypeExpr::Effect { input_type, output_type, .. } => {
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
            Declaration::Function { parameters, return_type, body, .. } => {
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
            literal: Literal::Integer { value: 42, span },
            span,
        };
        
        assert_eq!(expr.span(), span);
    }

    #[test]
    fn test_statement_span() {
        let span = Span::new(SourceLocation::start(), SourceLocation::new(1, 1, 0));
        let stmt = Statement::Expression {
            expression: Expression::Literal {
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
            expression: Expression::Literal {
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
        let str_lit = Literal::String { value: "hello".to_string(), span };
        
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
            identifier: Identifier::new("x".to_string(), span),
            span,
        };
        let bin_op = Expression::BinaryOp {
            left: Box::new(var_expr.clone()),
            operator: BinaryOperator::Add,
            right: Box::new(Expression::Literal {
                literal: Literal::Integer { value: 1, span },
                span,
            }),
            span,
        };
        
        assert_eq!(var_expr.to_string(), "x");
        assert_eq!(bin_op.to_string(), "x + 1");
    }
} 