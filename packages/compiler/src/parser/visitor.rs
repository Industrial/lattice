//! Visitor pattern implementation for AST traversal and manipulation.
//!
//! This module provides the visitor pattern for traversing and manipulating
//! the Abstract Syntax Tree (AST) nodes in the Lattice language parser.

use crate::parser::ast::{
  AstNode, Binding, Declaration, EffectOperation, Expression, FunctionParameter, HandlerCase,
  MatchArm, Pattern, Statement, TypeExpr, TypeVariant,
};

/// Trait for immutable visitors that traverse the AST
pub trait Visitor {
  /// Visit an AST node
  fn visit_ast(&mut self, ast: &AstNode) {
    for statement in &ast.statements {
      self.visit_statement(statement);
    }
  }

  /// Visit a statement
  fn visit_statement(&mut self, statement: &Statement) {
    match statement {
      Statement::Expression { expression, .. } => {
        self.visit_expression(expression);
      }
      Statement::Let { binding, .. } => {
        self.visit_binding(binding);
      }
      Statement::Type {
        name,
        parameters,
        variants,
        ..
      } => {
        self.visit_identifier(name);
        for param in parameters {
          self.visit_identifier(param);
        }
        for variant in variants {
          self.visit_type_variant(variant);
        }
      }
      Statement::Effect {
        name, operations, ..
      } => {
        self.visit_identifier(name);
        for operation in operations {
          self.visit_effect_operation(operation);
        }
      }
      Statement::Function {
        name,
        parameters,
        return_type,
        body,
        ..
      } => {
        self.visit_identifier(name);
        for param in parameters {
          self.visit_function_parameter(param);
        }
        if let Some(ty) = return_type {
          self.visit_type_expr(ty);
        }
        self.visit_expression(body);
      }
    }
  }

  /// Visit a binding
  fn visit_binding(&mut self, binding: &Binding) {
    self.visit_pattern(&binding.pattern);
    self.visit_expression(&binding.expression);
    if let Some(ty) = &binding.type_annotation {
      self.visit_type_expr(ty);
    }
  }

  /// Visit a type variant
  fn visit_type_variant(&mut self, variant: &TypeVariant) {
    self.visit_identifier(&variant.name);
    for field in &variant.fields {
      self.visit_type_expr(field);
    }
  }

  /// Visit an effect operation
  fn visit_effect_operation(&mut self, operation: &EffectOperation) {
    self.visit_identifier(&operation.name);
    self.visit_type_expr(&operation.input_type);
    self.visit_type_expr(&operation.output_type);
  }

  /// Visit a function parameter
  fn visit_function_parameter(&mut self, param: &FunctionParameter) {
    self.visit_pattern(&param.pattern);
    if let Some(ty) = &param.type_annotation {
      self.visit_type_expr(ty);
    }
  }

  /// Visit an expression
  fn visit_expression(&mut self, expression: &Expression) {
    match expression {
      Expression::Literal { literal, .. } => {
        self.visit_literal(literal);
      }
      Expression::Variable { identifier, .. } => {
        self.visit_identifier(identifier);
      }
      Expression::Application {
        function,
        arguments,
        ..
      } => {
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
          self.visit_binding(binding);
        }
        self.visit_expression(body);
      }
      Expression::If {
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
        scrutinee, arms, ..
      } => {
        self.visit_expression(scrutinee);
        for arm in arms {
          self.visit_match_arm(arm);
        }
      }
      Expression::Lambda {
        parameters, body, ..
      } => {
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
      Expression::EffectOp {
        operation,
        arguments,
        ..
      } => {
        self.visit_identifier(operation);
        for arg in arguments {
          self.visit_expression(arg);
        }
      }
      Expression::Handler {
        expression, cases, ..
      } => {
        self.visit_expression(expression);
        for case in cases {
          self.visit_handler_case(case);
        }
      }
    }
  }

  /// Visit a pattern
  fn visit_pattern(&mut self, pattern: &Pattern) {
    match pattern {
      Pattern::Variable { identifier, .. } => {
        self.visit_identifier(identifier);
      }
      Pattern::Wildcard { .. } => {
        // No sub-patterns to visit
      }
      Pattern::Literal { literal, .. } => {
        self.visit_literal(literal);
      }
      Pattern::Constructor {
        constructor,
        arguments,
        ..
      } => {
        self.visit_identifier(constructor);
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
      Pattern::As {
        pattern,
        identifier,
        ..
      } => {
        self.visit_pattern(pattern);
        self.visit_identifier(identifier);
      }
    }
  }

  /// Visit a type expression
  fn visit_type_expr(&mut self, type_expr: &TypeExpr) {
    match type_expr {
      TypeExpr::Variable { .. } => {
        // No sub-types to visit
      }
      TypeExpr::Constructor { name, .. } => {
        self.visit_identifier(name);
      }
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
      TypeExpr::Generic {
        constructor,
        arguments,
        ..
      } => {
        self.visit_identifier(constructor);
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
    }
  }

  /// Visit a match arm
  fn visit_match_arm(&mut self, arm: &MatchArm) {
    self.visit_pattern(&arm.pattern);
    if let Some(guard) = &arm.guard {
      self.visit_expression(guard);
    }
    self.visit_expression(&arm.expression);
  }

  /// Visit a handler case
  fn visit_handler_case(&mut self, case: &HandlerCase) {
    self.visit_identifier(&case.operation);
    self.visit_pattern(&case.pattern);
    self.visit_expression(&case.expression);
  }

  /// Visit a declaration
  fn visit_declaration(&mut self, declaration: &Declaration) {
    match declaration {
      Declaration::Type {
        name,
        parameters,
        variants,
        ..
      } => {
        self.visit_identifier(name);
        for param in parameters {
          self.visit_identifier(param);
        }
        for variant in variants {
          self.visit_type_variant(variant);
        }
      }
      Declaration::Effect {
        name, operations, ..
      } => {
        self.visit_identifier(name);
        for operation in operations {
          self.visit_effect_operation(operation);
        }
      }
      Declaration::Function {
        name,
        parameters,
        return_type,
        body,
        ..
      } => {
        self.visit_identifier(name);
        for param in parameters {
          self.visit_function_parameter(param);
        }
        if let Some(ty) = return_type {
          self.visit_type_expr(ty);
        }
        self.visit_expression(body);
      }
    }
  }

  /// Visit an identifier
  fn visit_identifier(&mut self, _identifier: &crate::parser::ast::Identifier) {
    // Default implementation does nothing
  }

  /// Visit a literal
  fn visit_literal(&mut self, _literal: &crate::parser::ast::Literal) {
    // Default implementation does nothing
  }
}

/// Trait for mutable visitors that can modify the AST
pub trait MutableVisitor {
  /// Visit an AST node mutably
  fn visit_ast(&mut self, ast: &mut AstNode) {
    for statement in &mut ast.statements {
      self.visit_statement(statement);
    }
  }

  /// Visit a statement mutably
  fn visit_statement(&mut self, statement: &mut Statement) {
    match statement {
      Statement::Expression { expression, .. } => {
        self.visit_expression(expression);
      }
      Statement::Let { binding, .. } => {
        self.visit_binding(binding);
      }
      Statement::Type {
        name,
        parameters,
        variants,
        ..
      } => {
        self.visit_identifier(name);
        for param in parameters {
          self.visit_identifier(param);
        }
        for variant in variants {
          self.visit_type_variant(variant);
        }
      }
      Statement::Effect {
        name, operations, ..
      } => {
        self.visit_identifier(name);
        for operation in operations {
          self.visit_effect_operation(operation);
        }
      }
      Statement::Function {
        name,
        parameters,
        return_type,
        body,
        ..
      } => {
        self.visit_identifier(name);
        for param in parameters {
          self.visit_function_parameter(param);
        }
        if let Some(ty) = return_type {
          self.visit_type_expr(ty);
        }
        self.visit_expression(body);
      }
    }
  }

  /// Visit a binding mutably
  fn visit_binding(&mut self, binding: &mut Binding) {
    self.visit_pattern(&mut binding.pattern);
    self.visit_expression(&mut binding.expression);
    if let Some(ty) = &mut binding.type_annotation {
      self.visit_type_expr(ty);
    }
  }

  /// Visit a type variant mutably
  fn visit_type_variant(&mut self, variant: &mut TypeVariant) {
    self.visit_identifier(&mut variant.name);
    for field in &mut variant.fields {
      self.visit_type_expr(field);
    }
  }

  /// Visit an effect operation mutably
  fn visit_effect_operation(&mut self, operation: &mut EffectOperation) {
    self.visit_identifier(&mut operation.name);
    self.visit_type_expr(&mut operation.input_type);
    self.visit_type_expr(&mut operation.output_type);
  }

  /// Visit a function parameter mutably
  fn visit_function_parameter(&mut self, param: &mut FunctionParameter) {
    self.visit_pattern(&mut param.pattern);
    if let Some(ty) = &mut param.type_annotation {
      self.visit_type_expr(ty);
    }
  }

  /// Visit an expression mutably
  fn visit_expression(&mut self, expression: &mut Expression) {
    match expression {
      Expression::Literal { literal, .. } => {
        self.visit_literal(literal);
      }
      Expression::Variable { identifier, .. } => {
        self.visit_identifier(identifier);
      }
      Expression::Application {
        function,
        arguments,
        ..
      } => {
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
          self.visit_binding(binding);
        }
        self.visit_expression(body);
      }
      Expression::If {
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
        scrutinee, arms, ..
      } => {
        self.visit_expression(scrutinee);
        for arm in arms {
          self.visit_match_arm(arm);
        }
      }
      Expression::Lambda {
        parameters, body, ..
      } => {
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
      Expression::EffectOp {
        operation,
        arguments,
        ..
      } => {
        self.visit_identifier(operation);
        for arg in arguments {
          self.visit_expression(arg);
        }
      }
      Expression::Handler {
        expression, cases, ..
      } => {
        self.visit_expression(expression);
        for case in cases {
          self.visit_handler_case(case);
        }
      }
    }
  }

  /// Visit a pattern mutably
  fn visit_pattern(&mut self, pattern: &mut Pattern) {
    match pattern {
      Pattern::Variable { identifier, .. } => {
        self.visit_identifier(identifier);
      }
      Pattern::Wildcard { .. } => {
        // No sub-patterns to visit
      }
      Pattern::Literal { literal, .. } => {
        self.visit_literal(literal);
      }
      Pattern::Constructor {
        constructor,
        arguments,
        ..
      } => {
        self.visit_identifier(constructor);
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
      Pattern::As {
        pattern,
        identifier,
        ..
      } => {
        self.visit_pattern(pattern);
        self.visit_identifier(identifier);
      }
    }
  }

  /// Visit a type expression mutably
  fn visit_type_expr(&mut self, type_expr: &mut TypeExpr) {
    match type_expr {
      TypeExpr::Variable { .. } => {
        // No sub-types to visit
      }
      TypeExpr::Constructor { name, .. } => {
        self.visit_identifier(name);
      }
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
      TypeExpr::Generic {
        constructor,
        arguments,
        ..
      } => {
        self.visit_identifier(constructor);
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
    }
  }

  /// Visit a match arm mutably
  fn visit_match_arm(&mut self, arm: &mut MatchArm) {
    self.visit_pattern(&mut arm.pattern);
    if let Some(guard) = &mut arm.guard {
      self.visit_expression(guard);
    }
    self.visit_expression(&mut arm.expression);
  }

  /// Visit a handler case mutably
  fn visit_handler_case(&mut self, case: &mut HandlerCase) {
    self.visit_identifier(&mut case.operation);
    self.visit_pattern(&mut case.pattern);
    self.visit_expression(&mut case.expression);
  }

  /// Visit a declaration mutably
  fn visit_declaration(&mut self, declaration: &mut Declaration) {
    match declaration {
      Declaration::Type {
        name,
        parameters,
        variants,
        ..
      } => {
        self.visit_identifier(name);
        for param in parameters {
          self.visit_identifier(param);
        }
        for variant in variants {
          self.visit_type_variant(variant);
        }
      }
      Declaration::Effect {
        name, operations, ..
      } => {
        self.visit_identifier(name);
        for operation in operations {
          self.visit_effect_operation(operation);
        }
      }
      Declaration::Function {
        name,
        parameters,
        return_type,
        body,
        ..
      } => {
        self.visit_identifier(name);
        for param in parameters {
          self.visit_function_parameter(param);
        }
        if let Some(ty) = return_type {
          self.visit_type_expr(ty);
        }
        self.visit_expression(body);
      }
    }
  }

  /// Visit an identifier mutably
  fn visit_identifier(&mut self, _identifier: &mut crate::parser::ast::Identifier) {
    // Default implementation does nothing
  }

  /// Visit a literal mutably
  fn visit_literal(&mut self, _literal: &mut crate::parser::ast::Literal) {
    // Default implementation does nothing
  }
}

/// Example visitor that counts nodes in the AST
pub struct NodeCounter {
  /// Total number of nodes visited
  pub total_nodes: usize,
  /// Number of expressions visited
  pub expressions: usize,
  /// Number of statements visited
  pub statements: usize,
  /// Number of patterns visited
  pub patterns: usize,
  /// Number of type expressions visited
  pub type_exprs: usize,
  /// Number of identifiers visited
  pub identifiers: usize,
  /// Number of literals visited
  pub literals: usize,
}

impl NodeCounter {
  /// Create a new node counter
  pub fn new() -> Self {
    Self {
      total_nodes: 0,
      expressions: 0,
      statements: 0,
      patterns: 0,
      type_exprs: 0,
      identifiers: 0,
      literals: 0,
    }
  }

  /// Get a summary of the counts
  pub fn summary(&self) -> String {
    format!(
            "AST contains {} total nodes:\n  - {} expressions\n  - {} statements\n  - {} patterns\n  - {} type expressions\n  - {} identifiers\n  - {} literals",
            self.total_nodes,
            self.expressions,
            self.statements,
            self.patterns,
            self.type_exprs,
            self.identifiers,
            self.literals
        )
  }
}

impl Default for NodeCounter {
  fn default() -> Self {
    Self::new()
  }
}

impl Visitor for NodeCounter {
  fn visit_statement(&mut self, statement: &Statement) {
    self.total_nodes += 1;
    self.statements += 1;
    // Call the specific methods that the default implementation would call
    match statement {
      Statement::Expression { expression, .. } => {
        self.visit_expression(expression);
      }
      Statement::Let { binding, .. } => {
        self.visit_binding(binding);
      }
      Statement::Type {
        name,
        parameters,
        variants,
        ..
      } => {
        self.visit_identifier(name);
        for param in parameters {
          self.visit_identifier(param);
        }
        for variant in variants {
          self.visit_type_variant(variant);
        }
      }
      Statement::Effect {
        name, operations, ..
      } => {
        self.visit_identifier(name);
        for operation in operations {
          self.visit_effect_operation(operation);
        }
      }
      Statement::Function {
        name,
        parameters,
        return_type,
        body,
        ..
      } => {
        self.visit_identifier(name);
        for param in parameters {
          self.visit_function_parameter(param);
        }
        if let Some(ty) = return_type {
          self.visit_type_expr(ty);
        }
        self.visit_expression(body);
      }
    }
  }

  fn visit_expression(&mut self, expression: &Expression) {
    self.total_nodes += 1;
    self.expressions += 1;
    // Call the specific methods that the default implementation would call
    match expression {
      Expression::Literal { literal, .. } => {
        self.visit_literal(literal);
      }
      Expression::Variable { identifier, .. } => {
        self.visit_identifier(identifier);
      }
      Expression::Application {
        function,
        arguments,
        ..
      } => {
        self.visit_expression(function);
        for arg in arguments {
          self.visit_expression(arg);
        }
      }
      Expression::UnaryOp { operand, .. } => {
        self.visit_expression(operand);
      }
      Expression::BinaryOp { left, right, .. } => {
        self.visit_expression(left);
        self.visit_expression(right);
      }
      Expression::Let { bindings, body, .. } => {
        for binding in bindings {
          self.visit_binding(binding);
        }
        self.visit_expression(body);
      }
      Expression::If {
        condition,
        then_branch,
        else_branch,
        ..
      } => {
        self.visit_expression(condition);
        self.visit_expression(then_branch);
        self.visit_expression(else_branch);
      }
      Expression::Lambda {
        parameters, body, ..
      } => {
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
      Expression::Match {
        scrutinee, arms, ..
      } => {
        self.visit_expression(scrutinee);
        for arm in arms {
          self.visit_match_arm(arm);
        }
      }
      Expression::EffectOp {
        operation,
        arguments,
        ..
      } => {
        self.visit_identifier(operation);
        for arg in arguments {
          self.visit_expression(arg);
        }
      }
      Expression::Handler {
        expression, cases, ..
      } => {
        self.visit_expression(expression);
        for case in cases {
          self.visit_handler_case(case);
        }
      }
    }
  }

  fn visit_pattern(&mut self, pattern: &Pattern) {
    self.total_nodes += 1;
    self.patterns += 1;
    // Call the specific methods that the default implementation would call
    match pattern {
      Pattern::Variable { identifier, .. } => {
        self.visit_identifier(identifier);
      }
      Pattern::Wildcard { .. } => {}
      Pattern::Literal { literal, .. } => {
        self.visit_literal(literal);
      }
      Pattern::Constructor {
        constructor,
        arguments,
        ..
      } => {
        self.visit_identifier(constructor);
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
      Pattern::As {
        pattern,
        identifier,
        ..
      } => {
        self.visit_pattern(pattern);
        self.visit_identifier(identifier);
      }
    }
  }

  fn visit_type_expr(&mut self, type_expr: &TypeExpr) {
    self.total_nodes += 1;
    self.type_exprs += 1;
    // Call the specific methods that the default implementation would call
    match type_expr {
      TypeExpr::Variable { .. } => {
        // No sub-nodes to visit
      }
      TypeExpr::Constructor { name, .. } => {
        self.visit_identifier(name);
      }
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
      TypeExpr::Generic {
        constructor,
        arguments,
        ..
      } => {
        self.visit_identifier(constructor);
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
    }
  }
}

/// Example visitor that pretty-prints the AST
pub struct PrettyPrinter {
  /// Current indentation level
  indent_level: usize,
  /// Output buffer
  output: String,
}

impl PrettyPrinter {
  /// Create a new pretty printer
  pub fn new() -> Self {
    Self {
      indent_level: 0,
      output: String::new(),
    }
  }

  /// Get the formatted output
  pub fn output(self) -> String {
    self.output
  }

  /// Add indentation to the output
  fn indent(&mut self) {
    self.indent_level += 1;
  }

  /// Remove indentation from the output
  fn dedent(&mut self) {
    if self.indent_level > 0 {
      self.indent_level -= 1;
    }
  }

  /// Add a line with proper indentation
  fn add_line(&mut self, content: &str) {
    let indent = "  ".repeat(self.indent_level);
    self.output.push_str(&format!("{}{}\n", indent, content));
  }
}

impl Default for PrettyPrinter {
  fn default() -> Self {
    Self::new()
  }
}

impl Visitor for PrettyPrinter {
  fn visit_ast(&mut self, ast: &AstNode) {
    self.add_line("AST {");
    self.indent();
    for statement in &ast.statements {
      self.visit_statement(statement);
    }
    self.dedent();
    self.add_line("}");
  }

  fn visit_statement(&mut self, statement: &Statement) {
    match statement {
      Statement::Expression { expression, .. } => {
        self.add_line("Statement::Expression {");
        self.indent();
        self.visit_expression(expression);
        self.dedent();
        self.add_line("}");
      }
      Statement::Let { binding, .. } => {
        self.add_line("Statement::Let {");
        self.indent();
        self.visit_binding(binding);
        self.dedent();
        self.add_line("}");
      }
      Statement::Type {
        name,
        parameters,
        variants,
        ..
      } => {
        self.add_line("Statement::Type {");
        self.indent();
        self.add_line(&format!("name: {}", name));
        if !parameters.is_empty() {
          self.add_line("parameters: [");
          self.indent();
          for param in parameters {
            self.add_line(&format!("{},", param));
          }
          self.dedent();
          self.add_line("]");
        }
        self.add_line("variants: [");
        self.indent();
        for variant in variants {
          self.visit_type_variant(variant);
        }
        self.dedent();
        self.add_line("]");
        self.dedent();
        self.add_line("}");
      }
      Statement::Effect {
        name, operations, ..
      } => {
        self.add_line("Statement::Effect {");
        self.indent();
        self.add_line(&format!("name: {}", name));
        self.add_line("operations: [");
        self.indent();
        for operation in operations {
          self.visit_effect_operation(operation);
        }
        self.dedent();
        self.add_line("]");
        self.dedent();
        self.add_line("}");
      }
      Statement::Function {
        name,
        parameters,
        return_type,
        body,
        ..
      } => {
        self.add_line("Statement::Function {");
        self.indent();
        self.add_line(&format!("name: {}", name));
        self.add_line("parameters: [");
        self.indent();
        for param in parameters {
          self.visit_function_parameter(param);
        }
        self.dedent();
        self.add_line("]");
        if let Some(ty) = return_type {
          self.add_line("return_type: ");
          self.indent();
          self.visit_type_expr(ty);
          self.dedent();
        }
        self.add_line("body: ");
        self.indent();
        self.visit_expression(body);
        self.dedent();
        self.dedent();
        self.add_line("}");
      }
    }
  }

  fn visit_expression(&mut self, expression: &Expression) {
    match expression {
      Expression::Literal { literal, .. } => {
        self.add_line(&format!("Expression::Literal {{ literal: {} }}", literal));
      }
      Expression::Variable { identifier, .. } => {
        self.add_line(&format!(
          "Expression::Variable {{ identifier: {} }}",
          identifier
        ));
      }
      Expression::Application {
        function,
        arguments,
        ..
      } => {
        self.add_line("Expression::Application {");
        self.indent();
        self.add_line("function: ");
        self.indent();
        self.visit_expression(function);
        self.dedent();
        if !arguments.is_empty() {
          self.add_line("arguments: [");
          self.indent();
          for arg in arguments {
            self.visit_expression(arg);
          }
          self.dedent();
          self.add_line("]");
        }
        self.dedent();
        self.add_line("}");
      }
      Expression::BinaryOp {
        left,
        operator,
        right,
        ..
      } => {
        self.add_line(&format!(
          "Expression::BinaryOp {{ operator: {} }}",
          operator
        ));
        self.indent();
        self.add_line("left: ");
        self.indent();
        self.visit_expression(left);
        self.dedent();
        self.add_line("right: ");
        self.indent();
        self.visit_expression(right);
        self.dedent();
        self.dedent();
      }
      _ => {
        // For brevity, just show the variant name for other expressions
        self.add_line(&format!(
          "Expression::{:?} {{ ... }}",
          std::mem::discriminant(expression)
        ));
      }
    }
  }

  fn visit_pattern(&mut self, pattern: &Pattern) {
    match pattern {
      Pattern::Variable { identifier, .. } => {
        self.add_line(&format!(
          "Pattern::Variable {{ identifier: {} }}",
          identifier
        ));
      }
      Pattern::Wildcard { .. } => {
        self.add_line("Pattern::Wildcard");
      }
      Pattern::Literal { literal, .. } => {
        self.add_line(&format!("Pattern::Literal {{ literal: {} }}", literal));
      }
      _ => {
        self.add_line(&format!(
          "Pattern::{:?} {{ ... }}",
          std::mem::discriminant(pattern)
        ));
      }
    }
  }

  fn visit_type_expr(&mut self, type_expr: &TypeExpr) {
    match type_expr {
      TypeExpr::Variable { name, .. } => {
        self.add_line(&format!("TypeExpr::Variable {{ name: '{} }}", name));
      }
      TypeExpr::Constructor { name, .. } => {
        self.add_line(&format!("TypeExpr::Constructor {{ name: {} }}", name));
      }
      _ => {
        self.add_line(&format!(
          "TypeExpr::{:?} {{ ... }}",
          std::mem::discriminant(type_expr)
        ));
      }
    }
  }

  fn visit_identifier(&mut self, identifier: &crate::parser::ast::Identifier) {
    self.add_line(&format!("Identifier {{ name: \"{}\" }}", identifier.name));
  }

  fn visit_literal(&mut self, literal: &crate::parser::ast::Literal) {
    self.add_line(&format!("Literal {{ value: {} }}", literal));
  }
}

/// Example visitor that transforms the AST
pub struct AstTransformer {
  /// Whether any transformations were made
  pub transformed: bool,
}

impl AstTransformer {
  /// Create a new AST transformer
  pub fn new() -> Self {
    Self { transformed: false }
  }

  /// Check if any transformations were made
  pub fn was_transformed(&self) -> bool {
    self.transformed
  }
}

impl Default for AstTransformer {
  fn default() -> Self {
    Self::new()
  }
}

impl MutableVisitor for AstTransformer {
  // This is a basic transformer that could be extended to perform
  // actual transformations on the AST. For now, it just tracks
  // whether any transformations were attempted.

  fn visit_expression(&mut self, expression: &mut Expression) {
    // Example transformation: convert x + 0 to x
    if let Expression::BinaryOp {
      left,
      operator,
      right,
      ..
    } = expression
    {
      if let crate::parser::ast::BinaryOperator::Add = operator {
        if let Expression::Literal { literal, .. } = &**right {
          if let crate::parser::ast::Literal::Integer { value: 0, .. } = literal {
            // Replace the entire expression with the left operand
            *expression = std::mem::replace(
              &mut **left,
              Expression::Literal {
                literal: crate::parser::ast::Literal::Integer {
                  value: 0,
                  span: crate::parser::ast::Span::new(
                    crate::lexer::SourceLocation::start(),
                    crate::lexer::SourceLocation::start(),
                  ),
                },
                span: crate::parser::ast::Span::new(
                  crate::lexer::SourceLocation::start(),
                  crate::lexer::SourceLocation::start(),
                ),
              },
            );
            self.transformed = true;
            return;
          }
        }
      }
    }

    // Continue with the default implementation
    MutableVisitor::visit_expression(self, expression);
  }
}

/// Visitor that can replace specific identifiers in the AST
pub struct IdentifierReplacer {
  /// Map of old identifiers to new identifiers
  pub replacements: std::collections::HashMap<String, String>,
  /// Whether any replacements were made
  pub replaced: bool,
}

impl IdentifierReplacer {
  /// Create a new identifier replacer
  pub fn new() -> Self {
    Self {
      replacements: std::collections::HashMap::new(),
      replaced: false,
    }
  }

  /// Add a replacement rule
  pub fn add_replacement(&mut self, old: String, new: String) {
    self.replacements.insert(old, new);
  }

  /// Check if any replacements were made
  pub fn was_replaced(&self) -> bool {
    self.replaced
  }
}

impl Default for IdentifierReplacer {
  fn default() -> Self {
    Self::new()
  }
}

impl MutableVisitor for IdentifierReplacer {
  fn visit_identifier(&mut self, identifier: &mut crate::parser::ast::Identifier) {
    if let Some(new_name) = self.replacements.get(&identifier.name) {
      identifier.name = new_name.clone();
      self.replaced = true;
    }
  }
}

/// Example visitor that collects all identifiers from the AST
pub struct IdentifierCollector {
  /// All identifiers found in the AST
  pub identifiers: Vec<String>,
}

impl IdentifierCollector {
  /// Create a new identifier collector
  pub fn new() -> Self {
    Self {
      identifiers: Vec::new(),
    }
  }

  /// Get all unique identifiers
  pub fn unique_identifiers(&self) -> Vec<String> {
    let mut unique = self.identifiers.clone();
    unique.sort();
    unique.dedup();
    unique
  }

  /// Get a summary of the identifiers
  pub fn summary(&self) -> String {
    let unique = self.unique_identifiers();
    format!(
      "Found {} identifiers ({} unique):\n  - {}",
      self.identifiers.len(),
      unique.len(),
      unique.join(", ")
    )
  }
}

impl Default for IdentifierCollector {
  fn default() -> Self {
    Self::new()
  }
}

impl Visitor for IdentifierCollector {
  fn visit_identifier(&mut self, identifier: &crate::parser::ast::Identifier) {
    self.identifiers.push(identifier.name.clone());
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::SourceLocation;
  use crate::parser::ast::{AstNode, Expression, Identifier, Literal, Span, Statement};

  fn create_test_ast() -> AstNode {
    let span = Span::new(SourceLocation::start(), SourceLocation::start());
    let identifier = Identifier::new("x".to_string(), span);
    let literal = Literal::Integer { value: 42, span };

    let expression = Expression::Literal { literal, span };
    let statement = Statement::Expression { expression, span };

    AstNode::new(vec![statement], span)
  }

  #[test]
  fn test_node_counter() {
    let ast = create_test_ast();
    let mut counter = NodeCounter::new();

    counter.visit_ast(&ast);

    assert_eq!(counter.total_nodes, 3); // AST + Statement + Expression
    assert_eq!(counter.statements, 1);
    assert_eq!(counter.expressions, 1);
    assert_eq!(counter.literals, 1);
  }

  #[test]
  fn test_pretty_printer() {
    let ast = create_test_ast();
    let mut printer = PrettyPrinter::new();

    printer.visit_ast(&ast);
    let output = printer.output();

    assert!(output.contains("AST {"));
    assert!(output.contains("Statement::Expression"));
    assert!(output.contains("Expression::Literal"));
  }

  #[test]
  fn test_ast_transformer() {
    let mut ast = create_test_ast();
    let mut transformer = AstTransformer::new();

    transformer.visit_ast(&mut ast);

    // No transformations should be made for this simple AST
    assert!(!transformer.was_transformed());
  }

  #[test]
  fn test_visitor_default_implementation() {
    let ast = create_test_ast();
    let mut counter = NodeCounter::new();

    // This should work without implementing all methods
    counter.visit_ast(&ast);

    assert!(counter.total_nodes > 0);
  }

  #[test]
  fn test_identifier_collector() {
    let ast = create_test_ast();
    let mut collector = IdentifierCollector::new();

    collector.visit_ast(&ast);

    assert_eq!(collector.identifiers.len(), 1);
    assert_eq!(collector.identifiers[0], "x");
    assert_eq!(collector.unique_identifiers().len(), 1);
  }

  #[test]
  fn test_identifier_replacer() {
    let mut ast = create_test_ast();
    let mut replacer = IdentifierReplacer::new();
    replacer.add_replacement("x".to_string(), "y".to_string());

    replacer.visit_ast(&mut ast);

    assert!(replacer.was_replaced());
    // The identifier should have been replaced
    if let Statement::Expression { expression, .. } = &ast.statements[0] {
      if let Expression::Variable { identifier, .. } = expression {
        assert_eq!(identifier.name, "y");
      }
    }
  }
}
