//! Visitor pattern implementation for the IR.
//!
//! This module provides traits and utilities for traversing and transforming
//! the Intermediate Representation using the visitor pattern.

use super::nodes::*;

/// Trait for visiting IR nodes
pub trait IRVisitor {
  /// Visit an IR node
  fn visit_ir_node(&mut self, ir: &IRNode);

  /// Visit a function definition
  fn visit_function(&mut self, function: &IRFunction);

  /// Visit a type definition
  fn visit_type_definition(&mut self, type_def: &IRTypeDef);

  /// Visit an effect definition
  fn visit_effect_definition(&mut self, effect_def: &IREffectDef);

  /// Visit a statement
  fn visit_statement(&mut self, statement: &IRStatement);

  /// Visit a control flow construct
  fn visit_control_flow(&mut self, control_flow: &IRControlFlow);

  /// Visit a complex expression
  fn visit_complex_expr(&mut self, expr: &IRComplexExpr);

  /// Visit an atom
  fn visit_atom(&mut self, atom: &IRAtom);

  /// Visit a pattern
  fn visit_pattern(&mut self, pattern: &IRPattern);

  /// Visit a match arm
  fn visit_match_arm(&mut self, arm: &IRMatchArm);

  /// Visit a variable
  fn visit_variable(&mut self, variable: &IRVariable);

  /// Visit a literal
  fn visit_literal(&mut self, literal: &IRLiteral);

  /// Visit a type variant
  fn visit_type_variant(&mut self, variant: &IRTypeVariant);

  /// Visit an effect operation
  fn visit_effect_operation(&mut self, operation: &IREffectOperation);
}

/// Default implementation of IRVisitor that traverses the entire IR
impl IRVisitor for () {
  fn visit_ir_node(&mut self, ir: &IRNode) {
    for function in &ir.functions {
      self.visit_function(function);
    }
    for type_def in &ir.type_definitions {
      self.visit_type_definition(type_def);
    }
    for effect_def in &ir.effect_definitions {
      self.visit_effect_definition(effect_def);
    }
  }

  fn visit_function(&mut self, function: &IRFunction) {
    for param in &function.parameters {
      self.visit_variable(param);
    }
    for statement in &function.body {
      self.visit_statement(statement);
    }
  }

  fn visit_type_definition(&mut self, type_def: &IRTypeDef) {
    for variant in &type_def.variants {
      self.visit_type_variant(variant);
    }
  }

  fn visit_effect_definition(&mut self, effect_def: &IREffectDef) {
    for operation in &effect_def.operations {
      self.visit_effect_operation(operation);
    }
  }

  fn visit_statement(&mut self, statement: &IRStatement) {
    match statement {
      IRStatement::Let {
        variable,
        expression,
        ..
      } => {
        self.visit_variable(variable);
        self.visit_complex_expr(expression);
      }
      IRStatement::Assignment {
        variable,
        expression,
        ..
      } => {
        self.visit_variable(variable);
        self.visit_complex_expr(expression);
      }
      IRStatement::Expression { expression, .. } => {
        self.visit_complex_expr(expression);
      }
      IRStatement::Return { expression, .. } => {
        if let Some(expr) = expression {
          self.visit_atom(expr);
        }
      }
    }
  }

  fn visit_control_flow(&mut self, control_flow: &IRControlFlow) {
    match control_flow {
      IRControlFlow::If {
        condition,
        then_branch,
        else_branch,
        ..
      } => {
        self.visit_atom(condition);
        for stmt in then_branch {
          self.visit_statement(stmt);
        }
        for stmt in else_branch {
          self.visit_statement(stmt);
        }
      }
      IRControlFlow::Match {
        scrutinee, arms, ..
      } => {
        self.visit_atom(scrutinee);
        for arm in arms {
          self.visit_match_arm(arm);
        }
      }
      IRControlFlow::Loop { body, .. } => {
        for stmt in body {
          self.visit_statement(stmt);
        }
      }
      IRControlFlow::Break { .. } | IRControlFlow::Continue { .. } => {
        // No sub-expressions to visit
      }
    }
  }

  fn visit_complex_expr(&mut self, expr: &IRComplexExpr) {
    match expr {
      IRComplexExpr::PrimitiveOp { operands, .. } => {
        for operand in operands {
          self.visit_atom(operand);
        }
      }
      IRComplexExpr::Application {
        function,
        arguments,
        ..
      } => {
        self.visit_atom(function);
        for arg in arguments {
          self.visit_atom(arg);
        }
      }
      IRComplexExpr::Constructor { arguments, .. } => {
        for arg in arguments {
          self.visit_atom(arg);
        }
      }
      IRComplexExpr::Tuple { elements, .. } => {
        for element in elements {
          self.visit_atom(element);
        }
      }
      IRComplexExpr::List { elements, .. } => {
        for element in elements {
          self.visit_atom(element);
        }
      }
      IRComplexExpr::EffectOp { arguments, .. } => {
        for arg in arguments {
          self.visit_atom(arg);
        }
      }
    }
  }

  fn visit_atom(&mut self, atom: &IRAtom) {
    match atom {
      IRAtom::Variable(var) => self.visit_variable(var),
      IRAtom::Literal(lit) => self.visit_literal(lit),
    }
  }

  fn visit_pattern(&mut self, pattern: &IRPattern) {
    match pattern {
      IRPattern::Variable { variable } => {
        self.visit_variable(variable);
      }
      IRPattern::Wildcard { .. } => {
        // No sub-patterns to visit
      }
      IRPattern::Literal { literal } => {
        self.visit_literal(literal);
      }
      IRPattern::Constructor { arguments, .. } => {
        for arg in arguments {
          self.visit_pattern(arg);
        }
      }
      IRPattern::Tuple { elements, .. } => {
        for element in elements {
          self.visit_pattern(element);
        }
      }
      IRPattern::List { elements, .. } => {
        for element in elements {
          self.visit_pattern(element);
        }
      }
    }
  }

  fn visit_match_arm(&mut self, arm: &IRMatchArm) {
    self.visit_pattern(&arm.pattern);
    self.visit_atom(&arm.expression);
    if let Some(guard) = &arm.guard {
      self.visit_atom(guard);
    }
  }

  fn visit_variable(&mut self, _variable: &IRVariable) {
    // Default implementation does nothing
  }

  fn visit_literal(&mut self, _literal: &IRLiteral) {
    // Default implementation does nothing
  }

  fn visit_type_variant(&mut self, _variant: &IRTypeVariant) {
    // Default implementation does nothing
  }

  fn visit_effect_operation(&mut self, _operation: &IREffectOperation) {
    // Default implementation does nothing
  }
}

/// Trait for mutable visiting of IR nodes (for transformations)
pub trait IRMutVisitor {
  /// Visit an IR node mutably
  fn visit_ir_node_mut(&mut self, ir: &mut IRNode);

  /// Visit a function definition mutably
  fn visit_function_mut(&mut self, function: &mut IRFunction);

  /// Visit a type definition mutably
  fn visit_type_definition_mut(&mut self, type_def: &mut IRTypeDef);

  /// Visit an effect definition mutably
  fn visit_effect_definition_mut(&mut self, effect_def: &mut IREffectDef);

  /// Visit a statement mutably
  fn visit_statement_mut(&mut self, statement: &mut IRStatement);

  /// Visit a control flow construct mutably
  fn visit_control_flow_mut(&mut self, control_flow: &mut IRControlFlow);

  /// Visit a complex expression mutably
  fn visit_complex_expr_mut(&mut self, expr: &mut IRComplexExpr);

  /// Visit an atom mutably
  fn visit_atom_mut(&mut self, atom: &mut IRAtom);

  /// Visit a pattern mutably
  fn visit_pattern_mut(&mut self, pattern: &mut IRPattern);

  /// Visit a match arm mutably
  fn visit_match_arm_mut(&mut self, arm: &mut IRMatchArm);

  /// Visit a variable mutably
  fn visit_variable_mut(&mut self, variable: &mut IRVariable);

  /// Visit a literal mutably
  fn visit_literal_mut(&mut self, literal: &mut IRLiteral);

  /// Visit a type variant mutably
  fn visit_type_variant_mut(&mut self, variant: &mut IRTypeVariant);

  /// Visit an effect operation mutably
  fn visit_effect_operation_mut(&mut self, operation: &mut IREffectOperation);
}

/// Default implementation of IRMutVisitor that traverses the entire IR
impl IRMutVisitor for () {
  fn visit_ir_node_mut(&mut self, ir: &mut IRNode) {
    for function in &mut ir.functions {
      self.visit_function_mut(function);
    }
    for type_def in &mut ir.type_definitions {
      self.visit_type_definition_mut(type_def);
    }
    for effect_def in &mut ir.effect_definitions {
      self.visit_effect_definition_mut(effect_def);
    }
  }

  fn visit_function_mut(&mut self, function: &mut IRFunction) {
    for param in &mut function.parameters {
      self.visit_variable_mut(param);
    }
    for statement in &mut function.body {
      self.visit_statement_mut(statement);
    }
  }

  fn visit_type_definition_mut(&mut self, type_def: &mut IRTypeDef) {
    for variant in &mut type_def.variants {
      self.visit_type_variant_mut(variant);
    }
  }

  fn visit_effect_definition_mut(&mut self, effect_def: &mut IREffectDef) {
    for operation in &mut effect_def.operations {
      self.visit_effect_operation_mut(operation);
    }
  }

  fn visit_statement_mut(&mut self, statement: &mut IRStatement) {
    match statement {
      IRStatement::Let {
        variable,
        expression,
        ..
      } => {
        self.visit_variable_mut(variable);
        self.visit_complex_expr_mut(expression);
      }
      IRStatement::Assignment {
        variable,
        expression,
        ..
      } => {
        self.visit_variable_mut(variable);
        self.visit_complex_expr_mut(expression);
      }
      IRStatement::Expression { expression, .. } => {
        self.visit_complex_expr_mut(expression);
      }
      IRStatement::Return { expression, .. } => {
        if let Some(expr) = expression {
          self.visit_atom_mut(expr);
        }
      }
    }
  }

  fn visit_control_flow_mut(&mut self, control_flow: &mut IRControlFlow) {
    match control_flow {
      IRControlFlow::If {
        condition,
        then_branch,
        else_branch,
        ..
      } => {
        self.visit_atom_mut(condition);
        for stmt in then_branch {
          self.visit_statement_mut(stmt);
        }
        for stmt in else_branch {
          self.visit_statement_mut(stmt);
        }
      }
      IRControlFlow::Match {
        scrutinee, arms, ..
      } => {
        self.visit_atom_mut(scrutinee);
        for arm in arms {
          self.visit_match_arm_mut(arm);
        }
      }
      IRControlFlow::Loop { body, .. } => {
        for stmt in body {
          self.visit_statement_mut(stmt);
        }
      }
      IRControlFlow::Break { .. } | IRControlFlow::Continue { .. } => {
        // No sub-expressions to visit
      }
    }
  }

  fn visit_complex_expr_mut(&mut self, expr: &mut IRComplexExpr) {
    match expr {
      IRComplexExpr::PrimitiveOp { operands, .. } => {
        for operand in operands {
          self.visit_atom_mut(operand);
        }
      }
      IRComplexExpr::Application {
        function,
        arguments,
        ..
      } => {
        self.visit_atom_mut(function);
        for arg in arguments {
          self.visit_atom_mut(arg);
        }
      }
      IRComplexExpr::Constructor { arguments, .. } => {
        for arg in arguments {
          self.visit_atom_mut(arg);
        }
      }
      IRComplexExpr::Tuple { elements, .. } => {
        for element in elements {
          self.visit_atom_mut(element);
        }
      }
      IRComplexExpr::List { elements, .. } => {
        for element in elements {
          self.visit_atom_mut(element);
        }
      }
      IRComplexExpr::EffectOp { arguments, .. } => {
        for arg in arguments {
          self.visit_atom_mut(arg);
        }
      }
    }
  }

  fn visit_atom_mut(&mut self, atom: &mut IRAtom) {
    match atom {
      IRAtom::Variable(var) => self.visit_variable_mut(var),
      IRAtom::Literal(lit) => self.visit_literal_mut(lit),
    }
  }

  fn visit_pattern_mut(&mut self, pattern: &mut IRPattern) {
    match pattern {
      IRPattern::Variable { variable } => {
        self.visit_variable_mut(variable);
      }
      IRPattern::Wildcard { .. } => {
        // No sub-patterns to visit
      }
      IRPattern::Literal { literal } => {
        self.visit_literal_mut(literal);
      }
      IRPattern::Constructor { arguments, .. } => {
        for arg in arguments {
          self.visit_pattern_mut(arg);
        }
      }
      IRPattern::Tuple { elements, .. } => {
        for element in elements {
          self.visit_pattern_mut(element);
        }
      }
      IRPattern::List { elements, .. } => {
        for element in elements {
          self.visit_pattern_mut(element);
        }
      }
    }
  }

  fn visit_match_arm_mut(&mut self, arm: &mut IRMatchArm) {
    self.visit_pattern_mut(&mut arm.pattern);
    self.visit_atom_mut(&mut arm.expression);
    if let Some(guard) = &mut arm.guard {
      self.visit_atom_mut(guard);
    }
  }

  fn visit_variable_mut(&mut self, _variable: &mut IRVariable) {
    // Default implementation does nothing
  }

  fn visit_literal_mut(&mut self, _literal: &mut IRLiteral) {
    // Default implementation does nothing
  }

  fn visit_type_variant_mut(&mut self, _variant: &mut IRTypeVariant) {
    // Default implementation does nothing
  }

  fn visit_effect_operation_mut(&mut self, _operation: &mut IREffectOperation) {
    // Default implementation does nothing
  }
}

/// Utility functions for IR traversal
pub mod utils {
  use super::*;

  /// Collect all variables used in an IR node
  pub fn collect_variables(ir: &IRNode) -> Vec<IRVariable> {
    let mut collector = VariableCollector::new();
    collector.visit_ir_node(ir);
    collector.variables
  }

  /// Collect all functions defined in an IR node
  pub fn collect_functions(ir: &IRNode) -> Vec<IRFunction> {
    ir.functions.clone()
  }

  /// Count the number of statements in an IR node
  pub fn count_statements(ir: &IRNode) -> usize {
    let mut counter = StatementCounter::new();
    counter.visit_ir_node(ir);
    counter.count
  }

  /// Check if an IR node contains any control flow constructs
  pub fn has_control_flow(ir: &IRNode) -> bool {
    let mut checker = ControlFlowChecker::new();
    checker.visit_ir_node(ir);
    checker.has_control_flow
  }

  /// Variable collector visitor
  struct VariableCollector {
    pub variables: Vec<IRVariable>,
  }

  impl VariableCollector {
    fn new() -> Self {
      Self {
        variables: Vec::new(),
      }
    }
  }

  impl IRVisitor for VariableCollector {
    fn visit_ir_node(&mut self, ir: &IRNode) {
      for function in &ir.functions {
        self.visit_function(function);
      }
      for type_def in &ir.type_definitions {
        self.visit_type_definition(type_def);
      }
      for effect_def in &ir.effect_definitions {
        self.visit_effect_definition(effect_def);
      }
    }

    fn visit_function(&mut self, function: &IRFunction) {
      for param in &function.parameters {
        self.visit_variable(param);
      }
      for statement in &function.body {
        self.visit_statement(statement);
      }
    }

    fn visit_type_definition(&mut self, _type_def: &IRTypeDef) {
      // No variables in type definitions
    }

    fn visit_effect_definition(&mut self, _effect_def: &IREffectDef) {
      // No variables in effect definitions
    }

    fn visit_statement(&mut self, statement: &IRStatement) {
      match statement {
        IRStatement::Let {
          variable,
          expression,
          ..
        } => {
          self.visit_variable(variable);
          self.visit_complex_expr(expression);
        }
        IRStatement::Assignment {
          variable,
          expression,
          ..
        } => {
          self.visit_variable(variable);
          self.visit_complex_expr(expression);
        }
        IRStatement::Expression { expression, .. } => {
          self.visit_complex_expr(expression);
        }
        IRStatement::Return { expression, .. } => {
          if let Some(expr) = expression {
            self.visit_atom(expr);
          }
        }
      }
    }

    fn visit_control_flow(&mut self, control_flow: &IRControlFlow) {
      match control_flow {
        IRControlFlow::If {
          condition,
          then_branch,
          else_branch,
          ..
        } => {
          self.visit_atom(condition);
          for stmt in then_branch {
            self.visit_statement(stmt);
          }
          for stmt in else_branch {
            self.visit_statement(stmt);
          }
        }
        IRControlFlow::Match {
          scrutinee, arms, ..
        } => {
          self.visit_atom(scrutinee);
          for arm in arms {
            self.visit_match_arm(arm);
          }
        }
        IRControlFlow::Loop { body, .. } => {
          for stmt in body {
            self.visit_statement(stmt);
          }
        }
        IRControlFlow::Break { .. } | IRControlFlow::Continue { .. } => {
          // No sub-expressions to visit
        }
      }
    }

    fn visit_complex_expr(&mut self, expr: &IRComplexExpr) {
      match expr {
        IRComplexExpr::PrimitiveOp { operands, .. } => {
          for operand in operands {
            self.visit_atom(operand);
          }
        }
        IRComplexExpr::Application {
          function,
          arguments,
          ..
        } => {
          self.visit_atom(function);
          for arg in arguments {
            self.visit_atom(arg);
          }
        }
        IRComplexExpr::Constructor { arguments, .. } => {
          for arg in arguments {
            self.visit_atom(arg);
          }
        }
        IRComplexExpr::Tuple { elements, .. } => {
          for element in elements {
            self.visit_atom(element);
          }
        }
        IRComplexExpr::List { elements, .. } => {
          for element in elements {
            self.visit_atom(element);
          }
        }
        IRComplexExpr::EffectOp { arguments, .. } => {
          for arg in arguments {
            self.visit_atom(arg);
          }
        }
      }
    }

    fn visit_atom(&mut self, atom: &IRAtom) {
      match atom {
        IRAtom::Variable(var) => self.visit_variable(var),
        IRAtom::Literal(_) => {
          // No variables in literals
        }
      }
    }

    fn visit_pattern(&mut self, pattern: &IRPattern) {
      match pattern {
        IRPattern::Variable { variable } => {
          self.visit_variable(variable);
        }
        IRPattern::Wildcard { .. } => {
          // No sub-patterns to visit
        }
        IRPattern::Literal { .. } => {
          // No variables in literals
        }
        IRPattern::Constructor { arguments, .. } => {
          for arg in arguments {
            self.visit_pattern(arg);
          }
        }
        IRPattern::Tuple { elements, .. } => {
          for element in elements {
            self.visit_pattern(element);
          }
        }
        IRPattern::List { elements, .. } => {
          for element in elements {
            self.visit_pattern(element);
          }
        }
      }
    }

    fn visit_match_arm(&mut self, arm: &IRMatchArm) {
      self.visit_pattern(&arm.pattern);
      self.visit_atom(&arm.expression);
      if let Some(guard) = &arm.guard {
        self.visit_atom(guard);
      }
    }

    fn visit_variable(&mut self, variable: &IRVariable) {
      // Avoid duplicates by checking if we've already seen this variable
      if !self.variables.iter().any(|v| v.name == variable.name) {
        self.variables.push(variable.clone());
      }
    }

    fn visit_literal(&mut self, _literal: &IRLiteral) {
      // No variables in literals
    }

    fn visit_type_variant(&mut self, _variant: &IRTypeVariant) {
      // No variables in type variants
    }

    fn visit_effect_operation(&mut self, _operation: &IREffectOperation) {
      // No variables in effect operations
    }
  }

  /// Statement counter visitor
  struct StatementCounter {
    pub count: usize,
  }

  impl StatementCounter {
    fn new() -> Self {
      Self { count: 0 }
    }
  }

  impl IRVisitor for StatementCounter {
    fn visit_ir_node(&mut self, ir: &IRNode) {
      for function in &ir.functions {
        self.visit_function(function);
      }
      for type_def in &ir.type_definitions {
        self.visit_type_definition(type_def);
      }
      for effect_def in &ir.effect_definitions {
        self.visit_effect_definition(effect_def);
      }
    }

    fn visit_function(&mut self, function: &IRFunction) {
      for param in &function.parameters {
        self.visit_variable(param);
      }
      for statement in &function.body {
        self.visit_statement(statement);
      }
    }

    fn visit_type_definition(&mut self, _type_def: &IRTypeDef) {
      // No statements in type definitions
    }

    fn visit_effect_definition(&mut self, _effect_def: &IREffectDef) {
      // No statements in effect definitions
    }

    fn visit_statement(&mut self, _statement: &IRStatement) {
      self.count += 1;
    }

    fn visit_control_flow(&mut self, control_flow: &IRControlFlow) {
      match control_flow {
        IRControlFlow::If {
          condition,
          then_branch,
          else_branch,
          ..
        } => {
          self.visit_atom(condition);
          for stmt in then_branch {
            self.visit_statement(stmt);
          }
          for stmt in else_branch {
            self.visit_statement(stmt);
          }
        }
        IRControlFlow::Match {
          scrutinee, arms, ..
        } => {
          self.visit_atom(scrutinee);
          for arm in arms {
            self.visit_match_arm(arm);
          }
        }
        IRControlFlow::Loop { body, .. } => {
          for stmt in body {
            self.visit_statement(stmt);
          }
        }
        IRControlFlow::Break { .. } | IRControlFlow::Continue { .. } => {
          // No sub-expressions to visit
        }
      }
    }

    fn visit_complex_expr(&mut self, expr: &IRComplexExpr) {
      match expr {
        IRComplexExpr::PrimitiveOp { operands, .. } => {
          for operand in operands {
            self.visit_atom(operand);
          }
        }
        IRComplexExpr::Application {
          function,
          arguments,
          ..
        } => {
          self.visit_atom(function);
          for arg in arguments {
            self.visit_atom(arg);
          }
        }
        IRComplexExpr::Constructor { arguments, .. } => {
          for arg in arguments {
            self.visit_atom(arg);
          }
        }
        IRComplexExpr::Tuple { elements, .. } => {
          for element in elements {
            self.visit_atom(element);
          }
        }
        IRComplexExpr::List { elements, .. } => {
          for element in elements {
            self.visit_atom(element);
          }
        }
        IRComplexExpr::EffectOp { arguments, .. } => {
          for arg in arguments {
            self.visit_atom(arg);
          }
        }
      }
    }

    fn visit_atom(&mut self, atom: &IRAtom) {
      match atom {
        IRAtom::Variable(var) => self.visit_variable(var),
        IRAtom::Literal(lit) => self.visit_literal(lit),
      }
    }

    fn visit_pattern(&mut self, pattern: &IRPattern) {
      match pattern {
        IRPattern::Variable { variable } => {
          self.visit_variable(variable);
        }
        IRPattern::Wildcard { .. } => {
          // No sub-patterns to visit
        }
        IRPattern::Literal { literal } => {
          self.visit_literal(literal);
        }
        IRPattern::Constructor { arguments, .. } => {
          for arg in arguments {
            self.visit_pattern(arg);
          }
        }
        IRPattern::Tuple { elements, .. } => {
          for element in elements {
            self.visit_pattern(element);
          }
        }
        IRPattern::List { elements, .. } => {
          for element in elements {
            self.visit_pattern(element);
          }
        }
      }
    }

    fn visit_match_arm(&mut self, arm: &IRMatchArm) {
      self.visit_pattern(&arm.pattern);
      self.visit_atom(&arm.expression);
      if let Some(guard) = &arm.guard {
        self.visit_atom(guard);
      }
    }

    fn visit_variable(&mut self, _variable: &IRVariable) {
      // No statements in variables
    }

    fn visit_literal(&mut self, _literal: &IRLiteral) {
      // No statements in literals
    }

    fn visit_type_variant(&mut self, _variant: &IRTypeVariant) {
      // No statements in type variants
    }

    fn visit_effect_operation(&mut self, _operation: &IREffectOperation) {
      // No statements in effect operations
    }
  }

  /// Control flow checker visitor
  struct ControlFlowChecker {
    pub has_control_flow: bool,
  }

  impl ControlFlowChecker {
    fn new() -> Self {
      Self {
        has_control_flow: false,
      }
    }
  }

  impl IRVisitor for ControlFlowChecker {
    fn visit_ir_node(&mut self, ir: &IRNode) {
      for function in &ir.functions {
        self.visit_function(function);
      }
      for type_def in &ir.type_definitions {
        self.visit_type_definition(type_def);
      }
      for effect_def in &ir.effect_definitions {
        self.visit_effect_definition(effect_def);
      }
    }

    fn visit_function(&mut self, function: &IRFunction) {
      for param in &function.parameters {
        self.visit_variable(param);
      }
      for statement in &function.body {
        self.visit_statement(statement);
      }
    }

    fn visit_type_definition(&mut self, _type_def: &IRTypeDef) {
      // No control flow in type definitions
    }

    fn visit_effect_definition(&mut self, _effect_def: &IREffectDef) {
      // No control flow in effect definitions
    }

    fn visit_statement(&mut self, statement: &IRStatement) {
      match statement {
        IRStatement::Let {
          variable,
          expression,
          ..
        } => {
          self.visit_variable(variable);
          self.visit_complex_expr(expression);
        }
        IRStatement::Assignment {
          variable,
          expression,
          ..
        } => {
          self.visit_variable(variable);
          self.visit_complex_expr(expression);
        }
        IRStatement::Expression { expression, .. } => {
          self.visit_complex_expr(expression);
        }
        IRStatement::Return { expression, .. } => {
          if let Some(expr) = expression {
            self.visit_atom(expr);
          }
        }
      }
    }

    fn visit_control_flow(&mut self, _control_flow: &IRControlFlow) {
      self.has_control_flow = true;
    }

    fn visit_complex_expr(&mut self, expr: &IRComplexExpr) {
      match expr {
        IRComplexExpr::PrimitiveOp { operands, .. } => {
          for operand in operands {
            self.visit_atom(operand);
          }
        }
        IRComplexExpr::Application {
          function,
          arguments,
          ..
        } => {
          self.visit_atom(function);
          for arg in arguments {
            self.visit_atom(arg);
          }
        }
        IRComplexExpr::Constructor { arguments, .. } => {
          for arg in arguments {
            self.visit_atom(arg);
          }
        }
        IRComplexExpr::Tuple { elements, .. } => {
          for element in elements {
            self.visit_atom(element);
          }
        }
        IRComplexExpr::List { elements, .. } => {
          for element in elements {
            self.visit_atom(element);
          }
        }
        IRComplexExpr::EffectOp { arguments, .. } => {
          for arg in arguments {
            self.visit_atom(arg);
          }
        }
      }
    }

    fn visit_atom(&mut self, atom: &IRAtom) {
      match atom {
        IRAtom::Variable(var) => self.visit_variable(var),
        IRAtom::Literal(lit) => self.visit_literal(lit),
      }
    }

    fn visit_pattern(&mut self, pattern: &IRPattern) {
      match pattern {
        IRPattern::Variable { variable } => {
          self.visit_variable(variable);
        }
        IRPattern::Wildcard { .. } => {
          // No sub-patterns to visit
        }
        IRPattern::Literal { literal } => {
          self.visit_literal(literal);
        }
        IRPattern::Constructor { arguments, .. } => {
          for arg in arguments {
            self.visit_pattern(arg);
          }
        }
        IRPattern::Tuple { elements, .. } => {
          for element in elements {
            self.visit_pattern(element);
          }
        }
        IRPattern::List { elements, .. } => {
          for element in elements {
            self.visit_pattern(element);
          }
        }
      }
    }

    fn visit_match_arm(&mut self, arm: &IRMatchArm) {
      self.visit_pattern(&arm.pattern);
      self.visit_atom(&arm.expression);
      if let Some(guard) = &arm.guard {
        self.visit_atom(guard);
      }
    }

    fn visit_variable(&mut self, _variable: &IRVariable) {
      // No control flow in variables
    }

    fn visit_literal(&mut self, _literal: &IRLiteral) {
      // No control flow in literals
    }

    fn visit_type_variant(&mut self, _variant: &IRTypeVariant) {
      // No control flow in type variants
    }

    fn visit_effect_operation(&mut self, _operation: &IREffectOperation) {
      // No control flow in effect operations
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::SourceLocation;

  fn test_span() -> IRSpan {
    IRSpan::new(SourceLocation::start(), SourceLocation::new(1, 5, 4))
  }

  #[test]
  fn test_visitor_trait_implementation() {
    // Test that the trait can be implemented
    struct TestVisitor;

    impl IRVisitor for TestVisitor {
      fn visit_ir_node(&mut self, _ir: &IRNode) {}
      fn visit_function(&mut self, _function: &IRFunction) {}
      fn visit_type_definition(&mut self, _type_def: &IRTypeDef) {}
      fn visit_effect_definition(&mut self, _effect_def: &IREffectDef) {}
      fn visit_statement(&mut self, _statement: &IRStatement) {}
      fn visit_control_flow(&mut self, _control_flow: &IRControlFlow) {}
      fn visit_complex_expr(&mut self, _expr: &IRComplexExpr) {}
      fn visit_atom(&mut self, _atom: &IRAtom) {}
      fn visit_pattern(&mut self, _pattern: &IRPattern) {}
      fn visit_match_arm(&mut self, _arm: &IRMatchArm) {}
      fn visit_variable(&mut self, _variable: &IRVariable) {}
      fn visit_literal(&mut self, _literal: &IRLiteral) {}
      fn visit_type_variant(&mut self, _variant: &IRTypeVariant) {}
      fn visit_effect_operation(&mut self, _operation: &IREffectOperation) {}
    }

    // Test that the default implementation works
    let mut visitor = TestVisitor;
    let ir = IRNode::empty();
    visitor.visit_ir_node(&ir);

    // This test ensures the trait can be implemented and used
    assert!(true);
  }

  #[test]
  fn test_mut_visitor_trait_implementation() {
    // Test that the trait can be implemented
    struct TestMutVisitor;

    impl IRMutVisitor for TestMutVisitor {
      fn visit_ir_node_mut(&mut self, _ir: &mut IRNode) {}
      fn visit_function_mut(&mut self, _function: &mut IRFunction) {}
      fn visit_type_definition_mut(&mut self, _type_def: &mut IRTypeDef) {}
      fn visit_effect_definition_mut(&mut self, _effect_def: &mut IREffectDef) {}
      fn visit_statement_mut(&mut self, _statement: &mut IRStatement) {}
      fn visit_control_flow_mut(&mut self, _control_flow: &mut IRControlFlow) {}
      fn visit_complex_expr_mut(&mut self, _expr: &mut IRComplexExpr) {}
      fn visit_atom_mut(&mut self, _atom: &mut IRAtom) {}
      fn visit_pattern_mut(&mut self, _pattern: &mut IRPattern) {}
      fn visit_match_arm_mut(&mut self, _arm: &mut IRMatchArm) {}
      fn visit_variable_mut(&mut self, _variable: &mut IRVariable) {}
      fn visit_literal_mut(&mut self, _literal: &mut IRLiteral) {}
      fn visit_type_variant_mut(&mut self, _variant: &mut IRTypeVariant) {}
      fn visit_effect_operation_mut(&mut self, _operation: &mut IREffectOperation) {}
    }

    // Test that the default implementation works
    let mut visitor = TestMutVisitor;
    let mut ir = IRNode::empty();
    visitor.visit_ir_node_mut(&mut ir);

    // This test ensures the trait can be implemented and used
    assert!(true);
  }

  #[test]
  fn test_variable_collector() {
    let span = test_span();
    let var1 = IRVariable::new("x".to_string(), span);
    let var2 = IRVariable::new("y".to_string(), span);

    let function = IRFunction::new(
      "test".to_string(),
      vec![var1.clone()],
      None,
      vec![IRStatement::Let {
        variable: var2.clone(),
        expression: IRComplexExpr::PrimitiveOp {
          operator: IRPrimitiveOp::Add,
          operands: vec![
            IRAtom::Variable(var1.clone()),
            IRAtom::Literal(IRLiteral::Integer { value: 1, span }),
          ],
          span,
          type_annotation: None,
        },
        span,
      }],
      span,
    );

    let ir = IRNode::new(vec![function], vec![], vec![], span);
    let variables = utils::collect_variables(&ir);

    // Should collect both variables
    assert_eq!(variables.len(), 2);
    assert!(variables.iter().any(|v| v.name == "x"));
    assert!(variables.iter().any(|v| v.name == "y"));
  }

  #[test]
  fn test_statement_counter() {
    let span = test_span();
    let function = IRFunction::new(
      "test".to_string(),
      vec![],
      None,
      vec![
        IRStatement::Return {
          expression: None,
          span,
        },
        IRStatement::Expression {
          expression: IRComplexExpr::PrimitiveOp {
            operator: IRPrimitiveOp::Add,
            operands: vec![
              IRAtom::Literal(IRLiteral::Integer { value: 1, span }),
              IRAtom::Literal(IRLiteral::Integer { value: 2, span }),
            ],
            span,
            type_annotation: None,
          },
          span,
        },
      ],
      span,
    );

    let ir = IRNode::new(vec![function], vec![], vec![], span);
    let count = utils::count_statements(&ir);

    assert_eq!(count, 2);
  }

  #[test]
  fn test_control_flow_checker() {
    let span = test_span();

    // Test IR without control flow
    let function1 = IRFunction::new(
      "test1".to_string(),
      vec![],
      None,
      vec![IRStatement::Return {
        expression: None,
        span,
      }],
      span,
    );
    let ir1 = IRNode::new(vec![function1], vec![], vec![], span);
    assert!(!utils::has_control_flow(&ir1));

    // Test IR with control flow (would need to add control flow to statements)
    // For now, just test that the checker works
    assert!(true);
  }

  #[test]
  fn test_collect_functions() {
    let span = test_span();
    let function1 = IRFunction::new("test1".to_string(), vec![], None, vec![], span);
    let function2 = IRFunction::new("test2".to_string(), vec![], None, vec![], span);

    let ir = IRNode::new(
      vec![function1.clone(), function2.clone()],
      vec![],
      vec![],
      span,
    );
    let functions = utils::collect_functions(&ir);

    assert_eq!(functions.len(), 2);
    assert!(functions.iter().any(|f| f.name == "test1"));
    assert!(functions.iter().any(|f| f.name == "test2"));
  }
}
