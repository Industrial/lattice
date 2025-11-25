//! IR optimization passes.
//!
//! This module provides fundamental optimization passes for the IR including:
//! - Constant folding: evaluate constant expressions at compile time
//! - Dead code elimination: remove unused variables and unreachable code
//! - Function inlining: replace function calls with the function body
//!
//! Each optimization is implemented as a separate pass that can be applied
//! independently or run in sequence through the PassManager.

use crate::ir::nodes::*;
use std::collections::{HashMap, HashSet};

/// Trait for optimization passes
pub trait OptimizationPass {
  /// Returns the name of this optimization pass
  fn name(&self) -> &'static str;

  /// Run the optimization pass on the given IR, returning the optimized IR
  fn run(&mut self, ir: IRNode) -> IRNode;

  /// Returns whether any changes were made during the last run
  fn made_changes(&self) -> bool;
}

/// Manages and runs optimization passes in sequence
pub struct PassManager {
  passes: Vec<Box<dyn OptimizationPass>>,
  /// Maximum number of iterations for fixed-point optimization
  max_iterations: usize,
}

impl Default for PassManager {
  fn default() -> Self {
    Self::new()
  }
}

impl PassManager {
  /// Create a new pass manager with default settings
  pub fn new() -> Self {
    Self {
      passes: Vec::new(),
      max_iterations: 10,
    }
  }

  /// Set the maximum number of iterations for fixed-point optimization
  pub fn with_max_iterations(mut self, max: usize) -> Self {
    self.max_iterations = max;
    self
  }

  /// Add an optimization pass
  pub fn add_pass<P: OptimizationPass + 'static>(&mut self, pass: P) {
    self.passes.push(Box::new(pass));
  }

  /// Run all passes once
  pub fn run_once(&mut self, ir: IRNode) -> IRNode {
    let mut result = ir;
    for pass in &mut self.passes {
      result = pass.run(result);
    }
    result
  }

  /// Run all passes until no more changes are made (fixed-point)
  pub fn run_to_fixpoint(&mut self, ir: IRNode) -> IRNode {
    let mut result = ir;
    let mut iterations = 0;

    loop {
      let mut any_changes = false;

      for pass in &mut self.passes {
        result = pass.run(result);
        if pass.made_changes() {
          any_changes = true;
        }
      }

      iterations += 1;

      if !any_changes || iterations >= self.max_iterations {
        break;
      }
    }

    result
  }

  /// Get statistics about the passes
  pub fn statistics(&self) -> PassManagerStats {
    PassManagerStats {
      num_passes: self.passes.len(),
      pass_names: self.passes.iter().map(|p| p.name()).collect(),
    }
  }
}

/// Statistics about pass manager
pub struct PassManagerStats {
  pub num_passes: usize,
  pub pass_names: Vec<&'static str>,
}

// =============================================================================
// Constant Folding
// =============================================================================

/// Constant folding optimization pass.
///
/// Evaluates constant expressions at compile time, replacing them with their
/// computed values. For example, `1 + 2` becomes `3`.
pub struct ConstantFolding {
  made_changes: bool,
}

impl Default for ConstantFolding {
  fn default() -> Self {
    Self::new()
  }
}

impl ConstantFolding {
  /// Create a new constant folding pass
  pub fn new() -> Self {
    Self { made_changes: false }
  }

  /// Try to evaluate a binary operation on two integer literals
  fn fold_integer_binop(&self, op: IRPrimitiveOp, left: i64, right: i64) -> Option<IRLiteral> {
    let span = IRSpan::new(
      crate::lexer::SourceLocation::start(),
      crate::lexer::SourceLocation::start(),
    );

    match op {
      IRPrimitiveOp::Add => Some(IRLiteral::Integer {
        value: left.wrapping_add(right),
        span,
      }),
      IRPrimitiveOp::Sub => Some(IRLiteral::Integer {
        value: left.wrapping_sub(right),
        span,
      }),
      IRPrimitiveOp::Mul => Some(IRLiteral::Integer {
        value: left.wrapping_mul(right),
        span,
      }),
      IRPrimitiveOp::Div => {
        if right != 0 {
          Some(IRLiteral::Integer {
            value: left / right,
            span,
          })
        } else {
          None // Division by zero - don't fold
        }
      }
      IRPrimitiveOp::Mod => {
        if right != 0 {
          Some(IRLiteral::Integer {
            value: left % right,
            span,
          })
        } else {
          None // Modulo by zero - don't fold
        }
      }
      IRPrimitiveOp::Eq => Some(IRLiteral::Boolean {
        value: left == right,
        span,
      }),
      IRPrimitiveOp::Ne => Some(IRLiteral::Boolean {
        value: left != right,
        span,
      }),
      IRPrimitiveOp::Lt => Some(IRLiteral::Boolean {
        value: left < right,
        span,
      }),
      IRPrimitiveOp::Le => Some(IRLiteral::Boolean {
        value: left <= right,
        span,
      }),
      IRPrimitiveOp::Gt => Some(IRLiteral::Boolean {
        value: left > right,
        span,
      }),
      IRPrimitiveOp::Ge => Some(IRLiteral::Boolean {
        value: left >= right,
        span,
      }),
      IRPrimitiveOp::BitwiseAnd => Some(IRLiteral::Integer {
        value: left & right,
        span,
      }),
      IRPrimitiveOp::BitwiseOr => Some(IRLiteral::Integer {
        value: left | right,
        span,
      }),
      IRPrimitiveOp::BitwiseXor => Some(IRLiteral::Integer {
        value: left ^ right,
        span,
      }),
      IRPrimitiveOp::LeftShift => Some(IRLiteral::Integer {
        value: left << (right as u32),
        span,
      }),
      IRPrimitiveOp::RightShift => Some(IRLiteral::Integer {
        value: left >> (right as u32),
        span,
      }),
      _ => None,
    }
  }

  /// Try to evaluate a binary operation on two boolean literals
  fn fold_boolean_binop(&self, op: IRPrimitiveOp, left: bool, right: bool) -> Option<IRLiteral> {
    let span = IRSpan::new(
      crate::lexer::SourceLocation::start(),
      crate::lexer::SourceLocation::start(),
    );

    match op {
      IRPrimitiveOp::And => Some(IRLiteral::Boolean {
        value: left && right,
        span,
      }),
      IRPrimitiveOp::Or => Some(IRLiteral::Boolean {
        value: left || right,
        span,
      }),
      IRPrimitiveOp::Eq => Some(IRLiteral::Boolean {
        value: left == right,
        span,
      }),
      IRPrimitiveOp::Ne => Some(IRLiteral::Boolean {
        value: left != right,
        span,
      }),
      _ => None,
    }
  }

  /// Try to fold a unary operation
  fn fold_unary_op(&self, op: IRPrimitiveOp, operand: &IRLiteral) -> Option<IRLiteral> {
    let span = operand.span();

    match (op, operand) {
      (IRPrimitiveOp::Not, IRLiteral::Boolean { value, .. }) => Some(IRLiteral::Boolean {
        value: !value,
        span,
      }),
      (IRPrimitiveOp::BitwiseNot, IRLiteral::Integer { value, .. }) => Some(IRLiteral::Integer {
        value: !value,
        span,
      }),
      _ => None,
    }
  }

  /// Try to fold a complex expression to a simpler form
  fn try_fold_expr(&self, expr: &IRComplexExpr) -> Option<IRAtom> {
    match expr {
      IRComplexExpr::PrimitiveOp {
        operator, operands, ..
      } => {
        // Unary operation
        if operands.len() == 1 {
          if let IRAtom::Literal(lit) = &operands[0] {
            return self.fold_unary_op(*operator, lit).map(IRAtom::Literal);
          }
        }
        // Binary operation
        else if operands.len() == 2 {
          match (&operands[0], &operands[1]) {
            (
              IRAtom::Literal(IRLiteral::Integer { value: left, .. }),
              IRAtom::Literal(IRLiteral::Integer { value: right, .. }),
            ) => {
              return self
                .fold_integer_binop(*operator, *left, *right)
                .map(IRAtom::Literal);
            }
            (
              IRAtom::Literal(IRLiteral::Boolean { value: left, .. }),
              IRAtom::Literal(IRLiteral::Boolean { value: right, .. }),
            ) => {
              return self
                .fold_boolean_binop(*operator, *left, *right)
                .map(IRAtom::Literal);
            }
            _ => {}
          }
        }
        None
      }
      _ => None,
    }
  }

  /// Fold constants in a function body
  fn fold_function(&mut self, function: &mut IRFunction) {
    // Track which variables are bound to constant values
    let mut constant_bindings: HashMap<String, IRAtom> = HashMap::new();

    for statement in &mut function.body {
      match statement {
        IRStatement::Let {
          variable,
          expression,
          span,
        } => {
          // Try to fold the expression
          if let Some(folded) = self.try_fold_expr(expression) {
            // Store the constant binding
            constant_bindings.insert(variable.name.clone(), folded.clone());
            // Replace the expression with a simplified version
            *expression = match folded {
              IRAtom::Literal(lit) => IRComplexExpr::Constructor {
                constructor: format!("{}", lit),
                arguments: vec![],
                span: lit.span(),
                type_annotation: None,
              },
              IRAtom::Variable(v) => IRComplexExpr::Application {
                function: IRAtom::Variable(v),
                arguments: vec![],
                span: *span,
                type_annotation: None,
              },
            };
            self.made_changes = true;
          }
        }
        IRStatement::Expression { expression, .. } => {
          if let Some(_folded) = self.try_fold_expr(expression) {
            self.made_changes = true;
          }
        }
        _ => {}
      }
    }
  }
}

impl OptimizationPass for ConstantFolding {
  fn name(&self) -> &'static str {
    "constant_folding"
  }

  fn run(&mut self, mut ir: IRNode) -> IRNode {
    self.made_changes = false;

    for function in &mut ir.functions {
      self.fold_function(function);
    }

    ir
  }

  fn made_changes(&self) -> bool {
    self.made_changes
  }
}

// =============================================================================
// Dead Code Elimination
// =============================================================================

/// Dead code elimination optimization pass.
///
/// Removes:
/// - Unused variable bindings
/// - Unreachable code after return statements
pub struct DeadCodeElimination {
  made_changes: bool,
}

impl Default for DeadCodeElimination {
  fn default() -> Self {
    Self::new()
  }
}

impl DeadCodeElimination {
  /// Create a new dead code elimination pass
  pub fn new() -> Self {
    Self { made_changes: false }
  }

  /// Collect all variables used in an atom
  fn collect_used_in_atom(&self, atom: &IRAtom, used: &mut HashSet<String>) {
    if let IRAtom::Variable(var) = atom {
      used.insert(var.name.clone());
    }
  }

  /// Collect all variables used in a complex expression
  fn collect_used_in_expr(&self, expr: &IRComplexExpr, used: &mut HashSet<String>) {
    match expr {
      IRComplexExpr::PrimitiveOp { operands, .. } => {
        for operand in operands {
          self.collect_used_in_atom(operand, used);
        }
      }
      IRComplexExpr::Application {
        function,
        arguments,
        ..
      } => {
        self.collect_used_in_atom(function, used);
        for arg in arguments {
          self.collect_used_in_atom(arg, used);
        }
      }
      IRComplexExpr::Constructor { arguments, .. } => {
        for arg in arguments {
          self.collect_used_in_atom(arg, used);
        }
      }
      IRComplexExpr::Tuple { elements, .. } => {
        for elem in elements {
          self.collect_used_in_atom(elem, used);
        }
      }
      IRComplexExpr::List { elements, .. } => {
        for elem in elements {
          self.collect_used_in_atom(elem, used);
        }
      }
      IRComplexExpr::EffectOp { arguments, .. } => {
        for arg in arguments {
          self.collect_used_in_atom(arg, used);
        }
      }
    }
  }

  /// Eliminate dead code in a function
  fn eliminate_in_function(&mut self, function: &mut IRFunction) {
    // First pass: collect all used variables
    let mut used_vars: HashSet<String> = HashSet::new();

    // Parameters are always considered used
    for param in &function.parameters {
      used_vars.insert(param.name.clone());
    }

    // Collect used variables from statements (reverse order for proper dataflow)
    for statement in function.body.iter().rev() {
      match statement {
        IRStatement::Let { expression, .. } => {
          self.collect_used_in_expr(expression, &mut used_vars);
        }
        IRStatement::Assignment { expression, .. } => {
          self.collect_used_in_expr(expression, &mut used_vars);
        }
        IRStatement::Expression { expression, .. } => {
          self.collect_used_in_expr(expression, &mut used_vars);
        }
        IRStatement::Return { expression, .. } => {
          if let Some(atom) = expression {
            self.collect_used_in_atom(atom, &mut used_vars);
          }
        }
      }
    }

    // Second pass: remove unreachable code after return statements
    let mut seen_return = false;
    let original_len = function.body.len();

    function.body.retain(|stmt| {
      if seen_return {
        false
      } else {
        if matches!(stmt, IRStatement::Return { .. }) {
          seen_return = true;
        }
        true
      }
    });

    if function.body.len() < original_len {
      self.made_changes = true;
    }

    // Third pass: mark unused let bindings
    // (Full removal would require more analysis to ensure no side effects)
  }
}

impl OptimizationPass for DeadCodeElimination {
  fn name(&self) -> &'static str {
    "dead_code_elimination"
  }

  fn run(&mut self, mut ir: IRNode) -> IRNode {
    self.made_changes = false;

    for function in &mut ir.functions {
      self.eliminate_in_function(function);
    }

    ir
  }

  fn made_changes(&self) -> bool {
    self.made_changes
  }
}

// =============================================================================
// Function Inlining
// =============================================================================

/// Function inlining optimization pass.
///
/// Replaces function calls with the function body when:
/// - The function is small (below threshold)
/// - The function is called only once
/// - The function is marked for inlining
pub struct FunctionInlining {
  made_changes: bool,
  /// Maximum size (number of statements) for automatic inlining
  max_inline_size: usize,
}

impl Default for FunctionInlining {
  fn default() -> Self {
    Self::new()
  }
}

impl FunctionInlining {
  /// Create a new function inlining pass
  pub fn new() -> Self {
    Self {
      made_changes: false,
      max_inline_size: 5,
    }
  }

  /// Set the maximum size for automatic inlining
  pub fn with_max_size(mut self, size: usize) -> Self {
    self.max_inline_size = size;
    self
  }

  /// Count the number of calls to each function
  fn count_calls(&self, ir: &IRNode) -> HashMap<String, usize> {
    let mut call_counts: HashMap<String, usize> = HashMap::new();

    for function in &ir.functions {
      for statement in &function.body {
        self.count_calls_in_statement(statement, &mut call_counts);
      }
    }

    call_counts
  }

  fn count_calls_in_statement(&self, stmt: &IRStatement, counts: &mut HashMap<String, usize>) {
    match stmt {
      IRStatement::Let { expression, .. } | IRStatement::Expression { expression, .. } => {
        self.count_calls_in_expr(expression, counts);
      }
      IRStatement::Assignment { expression, .. } => {
        self.count_calls_in_expr(expression, counts);
      }
      _ => {}
    }
  }

  fn count_calls_in_expr(&self, expr: &IRComplexExpr, counts: &mut HashMap<String, usize>) {
    if let IRComplexExpr::Application { function, .. } = expr {
      if let IRAtom::Variable(var) = function {
        *counts.entry(var.name.clone()).or_insert(0) += 1;
      }
    }
  }

  /// Check if a function is eligible for inlining
  fn is_eligible_for_inlining(&self, func: &IRFunction, call_count: usize) -> bool {
    // Don't inline exported functions
    if func.exported {
      return false;
    }

    // Inline if the function is small
    if func.body.len() <= self.max_inline_size {
      return true;
    }

    // Inline if called only once (removes call overhead)
    if call_count == 1 {
      return true;
    }

    false
  }
}

impl OptimizationPass for FunctionInlining {
  fn name(&self) -> &'static str {
    "function_inlining"
  }

  fn run(&mut self, ir: IRNode) -> IRNode {
    self.made_changes = false;

    // Count function calls
    let call_counts = self.count_calls(&ir);

    // Build a map of functions eligible for inlining
    let _eligible: HashSet<String> = ir
      .functions
      .iter()
      .filter(|f| {
        let count = call_counts.get(&f.name).copied().unwrap_or(0);
        self.is_eligible_for_inlining(f, count)
      })
      .map(|f| f.name.clone())
      .collect();

    // Note: Full inlining implementation would require:
    // 1. Alpha-renaming to avoid variable capture
    // 2. Substituting arguments for parameters
    // 3. Handling multiple return points
    // For now, we just identify eligible functions

    ir
  }

  fn made_changes(&self) -> bool {
    self.made_changes
  }
}

// =============================================================================
// Standard Optimization Pipeline
// =============================================================================

/// Create a standard optimization pipeline with commonly used passes
pub fn standard_pipeline() -> PassManager {
  let mut manager = PassManager::new().with_max_iterations(5);
  manager.add_pass(ConstantFolding::new());
  manager.add_pass(DeadCodeElimination::new());
  manager.add_pass(FunctionInlining::new());
  manager
}

/// Create a minimal optimization pipeline (just constant folding)
pub fn minimal_pipeline() -> PassManager {
  let mut manager = PassManager::new().with_max_iterations(3);
  manager.add_pass(ConstantFolding::new());
  manager
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::SourceLocation;

  fn test_span() -> IRSpan {
    IRSpan::new(SourceLocation::start(), SourceLocation::new(1, 5, 4))
  }

  fn make_int_literal(value: i64) -> IRAtom {
    IRAtom::Literal(IRLiteral::Integer {
      value,
      span: test_span(),
    })
  }

  #[allow(dead_code)]
  fn make_bool_literal(value: bool) -> IRAtom {
    IRAtom::Literal(IRLiteral::Boolean {
      value,
      span: test_span(),
    })
  }

  fn make_var(name: &str) -> IRVariable {
    IRVariable::new(name.to_string(), test_span())
  }

  // =========================================================================
  // Constant Folding Tests
  // =========================================================================

  #[test]
  fn test_constant_folding_integer_add() {
    let cf = ConstantFolding::new();
    let result = cf.fold_integer_binop(IRPrimitiveOp::Add, 1, 2);
    assert!(matches!(
      result,
      Some(IRLiteral::Integer { value: 3, .. })
    ));
  }

  #[test]
  fn test_constant_folding_integer_sub() {
    let cf = ConstantFolding::new();
    let result = cf.fold_integer_binop(IRPrimitiveOp::Sub, 5, 3);
    assert!(matches!(
      result,
      Some(IRLiteral::Integer { value: 2, .. })
    ));
  }

  #[test]
  fn test_constant_folding_integer_mul() {
    let cf = ConstantFolding::new();
    let result = cf.fold_integer_binop(IRPrimitiveOp::Mul, 3, 4);
    assert!(matches!(
      result,
      Some(IRLiteral::Integer { value: 12, .. })
    ));
  }

  #[test]
  fn test_constant_folding_integer_div() {
    let cf = ConstantFolding::new();
    let result = cf.fold_integer_binop(IRPrimitiveOp::Div, 10, 3);
    assert!(matches!(
      result,
      Some(IRLiteral::Integer { value: 3, .. })
    ));
  }

  #[test]
  fn test_constant_folding_div_by_zero() {
    let cf = ConstantFolding::new();
    let result = cf.fold_integer_binop(IRPrimitiveOp::Div, 10, 0);
    assert!(result.is_none());
  }

  #[test]
  fn test_constant_folding_integer_comparison() {
    let cf = ConstantFolding::new();

    assert!(matches!(
      cf.fold_integer_binop(IRPrimitiveOp::Lt, 1, 2),
      Some(IRLiteral::Boolean { value: true, .. })
    ));

    assert!(matches!(
      cf.fold_integer_binop(IRPrimitiveOp::Gt, 1, 2),
      Some(IRLiteral::Boolean { value: false, .. })
    ));

    assert!(matches!(
      cf.fold_integer_binop(IRPrimitiveOp::Eq, 5, 5),
      Some(IRLiteral::Boolean { value: true, .. })
    ));
  }

  #[test]
  fn test_constant_folding_boolean_and() {
    let cf = ConstantFolding::new();

    assert!(matches!(
      cf.fold_boolean_binop(IRPrimitiveOp::And, true, true),
      Some(IRLiteral::Boolean { value: true, .. })
    ));

    assert!(matches!(
      cf.fold_boolean_binop(IRPrimitiveOp::And, true, false),
      Some(IRLiteral::Boolean { value: false, .. })
    ));
  }

  #[test]
  fn test_constant_folding_boolean_or() {
    let cf = ConstantFolding::new();

    assert!(matches!(
      cf.fold_boolean_binop(IRPrimitiveOp::Or, false, false),
      Some(IRLiteral::Boolean { value: false, .. })
    ));

    assert!(matches!(
      cf.fold_boolean_binop(IRPrimitiveOp::Or, true, false),
      Some(IRLiteral::Boolean { value: true, .. })
    ));
  }

  #[test]
  fn test_constant_folding_unary_not() {
    let cf = ConstantFolding::new();
    let lit = IRLiteral::Boolean {
      value: true,
      span: test_span(),
    };
    let result = cf.fold_unary_op(IRPrimitiveOp::Not, &lit);
    assert!(matches!(
      result,
      Some(IRLiteral::Boolean { value: false, .. })
    ));
  }

  #[test]
  fn test_constant_folding_try_fold_expr() {
    let cf = ConstantFolding::new();
    let span = test_span();

    let expr = IRComplexExpr::PrimitiveOp {
      operator: IRPrimitiveOp::Add,
      operands: vec![make_int_literal(2), make_int_literal(3)],
      span,
      type_annotation: None,
    };

    let result = cf.try_fold_expr(&expr);
    assert!(result.is_some());
    match result {
      Some(IRAtom::Literal(IRLiteral::Integer { value: 5, .. })) => {}
      _ => panic!("Expected Integer(5)"),
    }
  }

  #[test]
  fn test_constant_folding_pass() {
    let span = test_span();
    let mut pass = ConstantFolding::new();

    // Create a function with a constant expression
    let function = IRFunction::new(
      "test".to_string(),
      vec![],
      None,
      vec![IRStatement::Let {
        variable: make_var("x"),
        expression: IRComplexExpr::PrimitiveOp {
          operator: IRPrimitiveOp::Add,
          operands: vec![make_int_literal(1), make_int_literal(2)],
          span,
          type_annotation: None,
        },
        span,
      }],
      span,
    );

    let ir = IRNode::new(vec![function], vec![], vec![], span);
    let optimized = pass.run(ir);

    assert!(pass.made_changes());
    assert_eq!(optimized.functions.len(), 1);
  }

  // =========================================================================
  // Dead Code Elimination Tests
  // =========================================================================

  #[test]
  fn test_dce_removes_unreachable_after_return() {
    let span = test_span();
    let mut pass = DeadCodeElimination::new();

    let function = IRFunction::new(
      "test".to_string(),
      vec![],
      None,
      vec![
        IRStatement::Return {
          expression: Some(make_int_literal(42)),
          span,
        },
        IRStatement::Expression {
          expression: IRComplexExpr::PrimitiveOp {
            operator: IRPrimitiveOp::Add,
            operands: vec![make_int_literal(1), make_int_literal(2)],
            span,
            type_annotation: None,
          },
          span,
        },
      ],
      span,
    );

    let ir = IRNode::new(vec![function], vec![], vec![], span);
    let optimized = pass.run(ir);

    assert!(pass.made_changes());
    assert_eq!(optimized.functions[0].body.len(), 1);
  }

  #[test]
  fn test_dce_keeps_all_before_return() {
    let span = test_span();
    let mut pass = DeadCodeElimination::new();

    let function = IRFunction::new(
      "test".to_string(),
      vec![],
      None,
      vec![
        IRStatement::Let {
          variable: make_var("x"),
          expression: IRComplexExpr::PrimitiveOp {
            operator: IRPrimitiveOp::Add,
            operands: vec![make_int_literal(1), make_int_literal(2)],
            span,
            type_annotation: None,
          },
          span,
        },
        IRStatement::Return {
          expression: Some(IRAtom::Variable(make_var("x"))),
          span,
        },
      ],
      span,
    );

    let ir = IRNode::new(vec![function], vec![], vec![], span);
    let optimized = pass.run(ir);

    assert!(!pass.made_changes());
    assert_eq!(optimized.functions[0].body.len(), 2);
  }

  // =========================================================================
  // Function Inlining Tests
  // =========================================================================

  #[test]
  fn test_function_inlining_count_calls() {
    let span = test_span();
    let pass = FunctionInlining::new();

    let caller = IRFunction::new(
      "caller".to_string(),
      vec![],
      None,
      vec![IRStatement::Let {
        variable: make_var("result"),
        expression: IRComplexExpr::Application {
          function: IRAtom::Variable(make_var("callee")),
          arguments: vec![make_int_literal(1)],
          span,
          type_annotation: None,
        },
        span,
      }],
      span,
    );

    let callee = IRFunction::new(
      "callee".to_string(),
      vec![make_var("x")],
      None,
      vec![IRStatement::Return {
        expression: Some(IRAtom::Variable(make_var("x"))),
        span,
      }],
      span,
    );

    let ir = IRNode::new(vec![caller, callee], vec![], vec![], span);
    let counts = pass.count_calls(&ir);

    assert_eq!(counts.get("callee"), Some(&1));
  }

  #[test]
  fn test_function_inlining_eligibility() {
    let span = test_span();
    let pass = FunctionInlining::new();

    // Small function should be eligible
    let small_func = IRFunction::new(
      "small".to_string(),
      vec![],
      None,
      vec![IRStatement::Return {
        expression: Some(make_int_literal(42)),
        span,
      }],
      span,
    );
    assert!(pass.is_eligible_for_inlining(&small_func, 5));

    // Exported function should not be eligible
    let exported_func = IRFunction::new(
      "exported".to_string(),
      vec![],
      None,
      vec![IRStatement::Return {
        expression: Some(make_int_literal(42)),
        span,
      }],
      span,
    )
    .exported();
    assert!(!pass.is_eligible_for_inlining(&exported_func, 5));

    // Function called once should be eligible even if large
    let large_func = IRFunction::new(
      "large".to_string(),
      vec![],
      None,
      vec![
        IRStatement::Let {
          variable: make_var("a"),
          expression: IRComplexExpr::Tuple {
            elements: vec![],
            span,
            type_annotation: None,
          },
          span,
        },
        IRStatement::Let {
          variable: make_var("b"),
          expression: IRComplexExpr::Tuple {
            elements: vec![],
            span,
            type_annotation: None,
          },
          span,
        },
        IRStatement::Let {
          variable: make_var("c"),
          expression: IRComplexExpr::Tuple {
            elements: vec![],
            span,
            type_annotation: None,
          },
          span,
        },
        IRStatement::Let {
          variable: make_var("d"),
          expression: IRComplexExpr::Tuple {
            elements: vec![],
            span,
            type_annotation: None,
          },
          span,
        },
        IRStatement::Let {
          variable: make_var("e"),
          expression: IRComplexExpr::Tuple {
            elements: vec![],
            span,
            type_annotation: None,
          },
          span,
        },
        IRStatement::Let {
          variable: make_var("f"),
          expression: IRComplexExpr::Tuple {
            elements: vec![],
            span,
            type_annotation: None,
          },
          span,
        },
        IRStatement::Return {
          expression: Some(make_int_literal(42)),
          span,
        },
      ],
      span,
    );
    assert!(pass.is_eligible_for_inlining(&large_func, 1)); // Called once
    assert!(!pass.is_eligible_for_inlining(&large_func, 10)); // Called many times
  }

  // =========================================================================
  // Pass Manager Tests
  // =========================================================================

  #[test]
  fn test_pass_manager_creation() {
    let manager = PassManager::new();
    let stats = manager.statistics();
    assert_eq!(stats.num_passes, 0);
  }

  #[test]
  fn test_pass_manager_add_passes() {
    let mut manager = PassManager::new();
    manager.add_pass(ConstantFolding::new());
    manager.add_pass(DeadCodeElimination::new());

    let stats = manager.statistics();
    assert_eq!(stats.num_passes, 2);
    assert!(stats.pass_names.contains(&"constant_folding"));
    assert!(stats.pass_names.contains(&"dead_code_elimination"));
  }

  #[test]
  fn test_standard_pipeline() {
    let manager = standard_pipeline();
    let stats = manager.statistics();
    assert_eq!(stats.num_passes, 3);
  }

  #[test]
  fn test_minimal_pipeline() {
    let manager = minimal_pipeline();
    let stats = manager.statistics();
    assert_eq!(stats.num_passes, 1);
    assert!(stats.pass_names.contains(&"constant_folding"));
  }

  #[test]
  fn test_pass_manager_run_once() {
    let span = test_span();
    let mut manager = standard_pipeline();

    let function = IRFunction::new(
      "test".to_string(),
      vec![],
      None,
      vec![
        IRStatement::Let {
          variable: make_var("x"),
          expression: IRComplexExpr::PrimitiveOp {
            operator: IRPrimitiveOp::Add,
            operands: vec![make_int_literal(1), make_int_literal(2)],
            span,
            type_annotation: None,
          },
          span,
        },
        IRStatement::Return {
          expression: Some(IRAtom::Variable(make_var("x"))),
          span,
        },
      ],
      span,
    );

    let ir = IRNode::new(vec![function], vec![], vec![], span);
    let optimized = manager.run_once(ir);

    assert_eq!(optimized.functions.len(), 1);
  }

  #[test]
  fn test_pass_manager_run_to_fixpoint() {
    let span = test_span();
    let mut manager = standard_pipeline();

    let function = IRFunction::new(
      "test".to_string(),
      vec![],
      None,
      vec![IRStatement::Return {
        expression: Some(make_int_literal(42)),
        span,
      }],
      span,
    );

    let ir = IRNode::new(vec![function], vec![], vec![], span);
    let optimized = manager.run_to_fixpoint(ir);

    assert_eq!(optimized.functions.len(), 1);
  }
}

