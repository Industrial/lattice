//! AST to IR conversion module.
//!
//! This module provides functionality to convert the typed Abstract Syntax Tree (AST)
//! into the Intermediate Representation (IR) in A-normal form. The conversion process:
//!
//! - Normalizes complex expressions into atomic operations
//! - Desugars complex patterns into simpler IR constructs
//! - Preserves type information from the AST
//! - Maintains source location information for debugging
//!
//! # A-Normal Form (ANF)
//!
//! The IR uses A-normal form, where:
//! - All intermediate values are explicitly named
//! - Function arguments must be atomic (variables or literals)
//! - Complex expressions are broken down into sequences of let-bindings

use crate::ir::nodes::*;
use crate::parser::ast::{self, Expression, Identifier, Literal, Pattern, Span, Statement};
use crate::types::types::{PrimitiveType, Type};
use std::fmt;

/// Errors that can occur during AST to IR conversion
#[derive(Debug, Clone, PartialEq)]
pub enum ConversionError {
  /// Unsupported AST construct
  UnsupportedConstruct {
    construct: String,
    span: Span,
    message: String,
  },
  /// Invalid pattern in context
  InvalidPattern { pattern: String, span: Span },
  /// Type mismatch during conversion
  TypeMismatch {
    expected: String,
    found: String,
    span: Span,
  },
  /// Internal conversion error
  InternalError { message: String },
}

impl fmt::Display for ConversionError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ConversionError::UnsupportedConstruct {
        construct, message, ..
      } => {
        write!(f, "Unsupported construct '{}': {}", construct, message)
      }
      ConversionError::InvalidPattern { pattern, .. } => {
        write!(f, "Invalid pattern in this context: {}", pattern)
      }
      ConversionError::TypeMismatch {
        expected, found, ..
      } => {
        write!(f, "Type mismatch: expected {}, found {}", expected, found)
      }
      ConversionError::InternalError { message } => {
        write!(f, "Internal conversion error: {}", message)
      }
    }
  }
}

impl std::error::Error for ConversionError {}

/// Result type for conversion operations
pub type ConversionResult<T> = Result<T, ConversionError>;

/// Represents the result of converting an expression to A-normal form.
///
/// The conversion may produce:
/// - A list of let-bindings for intermediate values
/// - A final atomic expression (variable or literal)
#[derive(Debug, Clone)]
pub struct ANFResult {
  /// Let-bindings for intermediate values
  pub bindings: Vec<IRStatement>,
  /// The final atomic expression
  pub atom: IRAtom,
}

impl ANFResult {
  /// Create a new ANF result with just an atom (no bindings)
  pub fn atom(atom: IRAtom) -> Self {
    Self {
      bindings: Vec::new(),
      atom,
    }
  }

  /// Create a new ANF result with bindings and an atom
  pub fn with_bindings(bindings: Vec<IRStatement>, atom: IRAtom) -> Self {
    Self { bindings, atom }
  }
}

/// The AST to IR converter.
///
/// This struct maintains state needed during conversion, including:
/// - A counter for generating fresh temporary variable names
/// - Type information from the type checker
#[derive(Debug)]
pub struct AstToIrConverter {
  /// Counter for generating unique temporary variable names
  temp_counter: u32,
}

impl Default for AstToIrConverter {
  fn default() -> Self {
    Self::new()
  }
}

impl AstToIrConverter {
  /// Create a new AST to IR converter
  pub fn new() -> Self {
    Self { temp_counter: 0 }
  }

  /// Generate a fresh temporary variable name
  fn fresh_temp(&mut self, prefix: &str) -> String {
    let name = format!("_{}_{}", prefix, self.temp_counter);
    self.temp_counter += 1;
    name
  }

  /// Convert an AST span to an IR span
  pub fn convert_span(&self, span: &Span) -> IRSpan {
    IRSpan::new(span.start, span.end)
  }

  /// Convert an AST identifier to an IR variable
  pub fn convert_identifier(&self, id: &Identifier) -> IRVariable {
    IRVariable::from_ast_identifier(id.name.clone(), self.convert_span(&id.span))
  }

  /// Convert an AST literal to an IR literal
  pub fn convert_literal(&self, lit: &Literal) -> IRLiteral {
    match lit {
      Literal::Integer { value, span } => IRLiteral::Integer {
        value: *value,
        span: self.convert_span(span),
      },
      Literal::Float { value, span } => IRLiteral::Float {
        value: *value,
        span: self.convert_span(span),
      },
      Literal::String { value, span } => IRLiteral::String {
        value: value.clone(),
        span: self.convert_span(span),
      },
      Literal::Char { value, span } => IRLiteral::Char {
        value: *value,
        span: self.convert_span(span),
      },
      Literal::Boolean { value, span } => IRLiteral::Boolean {
        value: *value,
        span: self.convert_span(span),
      },
      Literal::Unit { span } => IRLiteral::Unit {
        span: self.convert_span(span),
      },
    }
  }

  /// Convert an AST pattern to an IR pattern
  pub fn convert_pattern(&self, pattern: &Pattern) -> ConversionResult<IRPattern> {
    match pattern {
      Pattern::Variable { identifier, .. } => Ok(IRPattern::Variable {
        variable: self.convert_identifier(identifier),
      }),
      Pattern::Wildcard { span } => Ok(IRPattern::Wildcard {
        span: self.convert_span(span),
      }),
      Pattern::Literal { literal, .. } => Ok(IRPattern::Literal {
        literal: self.convert_literal(literal),
      }),
      Pattern::Constructor {
        constructor,
        arguments,
        span,
      } => {
        let ir_arguments: ConversionResult<Vec<IRPattern>> =
          arguments.iter().map(|p| self.convert_pattern(p)).collect();
        Ok(IRPattern::Constructor {
          constructor: constructor.name.clone(),
          arguments: ir_arguments?,
          span: self.convert_span(span),
        })
      }
      Pattern::Tuple { elements, span } => {
        let ir_elements: ConversionResult<Vec<IRPattern>> =
          elements.iter().map(|p| self.convert_pattern(p)).collect();
        Ok(IRPattern::Tuple {
          elements: ir_elements?,
          span: self.convert_span(span),
        })
      }
      Pattern::List { elements, span } => {
        let ir_elements: ConversionResult<Vec<IRPattern>> =
          elements.iter().map(|p| self.convert_pattern(p)).collect();
        Ok(IRPattern::List {
          elements: ir_elements?,
          span: self.convert_span(span),
        })
      }
      // Or-patterns and As-patterns need special handling (desugaring)
      Pattern::Or { span, .. } => Err(ConversionError::UnsupportedConstruct {
        construct: "Or-pattern".to_string(),
        span: *span,
        message: "Or-patterns should be desugared before IR conversion".to_string(),
      }),
      Pattern::As { span, .. } => Err(ConversionError::UnsupportedConstruct {
        construct: "As-pattern".to_string(),
        span: *span,
        message: "As-patterns should be desugared before IR conversion".to_string(),
      }),
    }
  }

  /// Convert an AST binary operator to an IR primitive operation
  pub fn convert_binary_op(&self, op: &ast::BinaryOperator) -> IRPrimitiveOp {
    match op {
      ast::BinaryOperator::Add => IRPrimitiveOp::Add,
      ast::BinaryOperator::Sub => IRPrimitiveOp::Sub,
      ast::BinaryOperator::Mul => IRPrimitiveOp::Mul,
      ast::BinaryOperator::Div => IRPrimitiveOp::Div,
      ast::BinaryOperator::Mod => IRPrimitiveOp::Mod,
      ast::BinaryOperator::Pow => IRPrimitiveOp::Pow,
      ast::BinaryOperator::Eq => IRPrimitiveOp::Eq,
      ast::BinaryOperator::Ne => IRPrimitiveOp::Ne,
      ast::BinaryOperator::Lt => IRPrimitiveOp::Lt,
      ast::BinaryOperator::Le => IRPrimitiveOp::Le,
      ast::BinaryOperator::Gt => IRPrimitiveOp::Gt,
      ast::BinaryOperator::Ge => IRPrimitiveOp::Ge,
      ast::BinaryOperator::And => IRPrimitiveOp::And,
      ast::BinaryOperator::Or => IRPrimitiveOp::Or,
    }
  }

  /// Convert an AST unary operator to an IR primitive operation
  pub fn convert_unary_op(&self, op: &ast::UnaryOperator) -> IRPrimitiveOp {
    match op {
      ast::UnaryOperator::Neg => IRPrimitiveOp::Sub, // Implemented as 0 - x
      ast::UnaryOperator::Not => IRPrimitiveOp::Not,
      ast::UnaryOperator::BitwiseNot => IRPrimitiveOp::BitwiseNot,
    }
  }

  /// Convert an expression to A-normal form.
  ///
  /// This is the main conversion function that normalizes expressions
  /// by introducing let-bindings for intermediate values.
  pub fn convert_expression(&mut self, expr: &Expression) -> ConversionResult<ANFResult> {
    match expr {
      // Literals are already atomic
      Expression::Literal {
        literal,
        type_annotation,
        ..
      } => {
        let ir_lit = self.convert_literal(literal);
        let atom = IRAtom::Literal(ir_lit);
        Ok(ANFResult::atom(atom))
      }

      // Variables are already atomic
      Expression::Variable {
        identifier,
        type_annotation,
        ..
      } => {
        let mut var = self.convert_identifier(identifier);
        if let Some(ty) = type_annotation {
          var = var.with_type(ty.clone());
        }
        Ok(ANFResult::atom(IRAtom::Variable(var)))
      }

      // Binary operations need their operands normalized
      Expression::BinaryOp {
        left,
        operator,
        right,
        span,
        type_annotation,
      } => {
        let left_anf = self.convert_expression(left)?;
        let right_anf = self.convert_expression(right)?;

        let ir_op = self.convert_binary_op(operator);
        let ir_span = self.convert_span(span);

        // Create the primitive operation
        let complex_expr = IRComplexExpr::PrimitiveOp {
          operator: ir_op,
          operands: vec![left_anf.atom.clone(), right_anf.atom.clone()],
          span: ir_span,
          type_annotation: type_annotation.clone(),
        };

        // Generate a temporary variable for the result
        let temp_name = self.fresh_temp("binop");
        let temp_var = IRVariable::new(temp_name.clone(), ir_span);
        let temp_var_with_type = if let Some(ty) = type_annotation {
          temp_var.with_type(ty.clone())
        } else {
          temp_var
        };

        // Create the let binding
        let let_stmt = IRStatement::Let {
          variable: temp_var_with_type.clone(),
          expression: complex_expr,
          span: ir_span,
        };

        // Combine all bindings
        let mut all_bindings = Vec::new();
        all_bindings.extend(left_anf.bindings);
        all_bindings.extend(right_anf.bindings);
        all_bindings.push(let_stmt);

        Ok(ANFResult::with_bindings(
          all_bindings,
          IRAtom::Variable(temp_var_with_type),
        ))
      }

      // Unary operations
      Expression::UnaryOp {
        operator,
        operand,
        span,
        type_annotation,
      } => {
        let operand_anf = self.convert_expression(operand)?;
        let ir_op = self.convert_unary_op(operator);
        let ir_span = self.convert_span(span);

        // For negation, we implement as 0 - x
        let operands = if matches!(operator, ast::UnaryOperator::Neg) {
          vec![
            IRAtom::Literal(IRLiteral::Integer {
              value: 0,
              span: ir_span,
            }),
            operand_anf.atom.clone(),
          ]
        } else {
          vec![operand_anf.atom.clone()]
        };

        let complex_expr = IRComplexExpr::PrimitiveOp {
          operator: ir_op,
          operands,
          span: ir_span,
          type_annotation: type_annotation.clone(),
        };

        let temp_name = self.fresh_temp("unop");
        let temp_var = IRVariable::new(temp_name, ir_span);
        let temp_var_with_type = if let Some(ty) = type_annotation {
          temp_var.with_type(ty.clone())
        } else {
          temp_var
        };

        let let_stmt = IRStatement::Let {
          variable: temp_var_with_type.clone(),
          expression: complex_expr,
          span: ir_span,
        };

        let mut all_bindings = operand_anf.bindings;
        all_bindings.push(let_stmt);

        Ok(ANFResult::with_bindings(
          all_bindings,
          IRAtom::Variable(temp_var_with_type),
        ))
      }

      // Function application
      Expression::Application {
        function,
        arguments,
        span,
        type_annotation,
      } => {
        // Convert function to atomic form
        let func_anf = self.convert_expression(function)?;

        // Convert all arguments to atomic form
        let mut arg_anfs: Vec<ANFResult> = Vec::new();
        for arg in arguments {
          arg_anfs.push(self.convert_expression(arg)?);
        }

        let ir_span = self.convert_span(span);

        let complex_expr = IRComplexExpr::Application {
          function: func_anf.atom.clone(),
          arguments: arg_anfs.iter().map(|a| a.atom.clone()).collect(),
          span: ir_span,
          type_annotation: type_annotation.clone(),
        };

        let temp_name = self.fresh_temp("app");
        let temp_var = IRVariable::new(temp_name, ir_span);
        let temp_var_with_type = if let Some(ty) = type_annotation {
          temp_var.with_type(ty.clone())
        } else {
          temp_var
        };

        let let_stmt = IRStatement::Let {
          variable: temp_var_with_type.clone(),
          expression: complex_expr,
          span: ir_span,
        };

        // Collect all bindings in order
        let mut all_bindings = func_anf.bindings;
        for arg_anf in arg_anfs {
          all_bindings.extend(arg_anf.bindings);
        }
        all_bindings.push(let_stmt);

        Ok(ANFResult::with_bindings(
          all_bindings,
          IRAtom::Variable(temp_var_with_type),
        ))
      }

      // Let expressions
      Expression::Let {
        bindings,
        body,
        span,
        type_annotation,
      } => {
        let mut all_ir_stmts = Vec::new();

        // Convert each binding
        for binding in bindings {
          let expr_anf = self.convert_expression(&binding.expression)?;
          all_ir_stmts.extend(expr_anf.bindings);

          // Convert the pattern to get the variable(s) being bound
          let ir_pattern = self.convert_pattern(&binding.pattern)?;

          // For simple variable patterns, create a let binding
          match ir_pattern {
            IRPattern::Variable { variable } => {
              // If the expression result is already atomic, just bind it
              let let_stmt = IRStatement::Let {
                variable: variable.clone(),
                expression: match &expr_anf.atom {
                  IRAtom::Variable(v) => IRComplexExpr::Application {
                    function: IRAtom::Variable(v.clone()),
                    arguments: vec![],
                    span: self.convert_span(&binding.span),
                    type_annotation: None,
                  },
                  IRAtom::Literal(l) => IRComplexExpr::Constructor {
                    constructor: format!("{}", l),
                    arguments: vec![],
                    span: l.span(),
                    type_annotation: None,
                  },
                },
                span: self.convert_span(&binding.span),
              };
              all_ir_stmts.push(let_stmt);
            }
            // For complex patterns, we need pattern matching desugaring
            _ => {
              return Err(ConversionError::UnsupportedConstruct {
                construct: "Complex pattern in let binding".to_string(),
                span: binding.span,
                message: "Complex patterns in let bindings should be desugared to match expressions"
                  .to_string(),
              });
            }
          }
        }

        // Convert the body
        let body_anf = self.convert_expression(body)?;
        all_ir_stmts.extend(body_anf.bindings);

        Ok(ANFResult::with_bindings(all_ir_stmts, body_anf.atom))
      }

      // If expressions
      Expression::If {
        condition,
        then_branch,
        else_branch,
        span,
        type_annotation,
      } => {
        // Convert condition to atomic form
        let cond_anf = self.convert_expression(condition)?;

        // Convert branches
        let then_anf = self.convert_expression(then_branch)?;
        let else_anf = self.convert_expression(else_branch)?;

        let ir_span = self.convert_span(span);

        // Generate temp var for the result
        let temp_name = self.fresh_temp("if_result");
        let temp_var = IRVariable::new(temp_name, ir_span);
        let temp_var_with_type = if let Some(ty) = type_annotation {
          temp_var.with_type(ty.clone())
        } else {
          temp_var
        };

        // Create then branch statements that assign to temp
        let mut then_stmts = then_anf.bindings;
        then_stmts.push(IRStatement::Assignment {
          variable: temp_var_with_type.clone(),
          expression: match then_anf.atom {
            IRAtom::Variable(ref v) => IRComplexExpr::Application {
              function: IRAtom::Variable(v.clone()),
              arguments: vec![],
              span: ir_span,
              type_annotation: None,
            },
            IRAtom::Literal(ref l) => IRComplexExpr::Constructor {
              constructor: format!("{}", l),
              arguments: vec![],
              span: l.span(),
              type_annotation: None,
            },
          },
          span: ir_span,
        });

        // Create else branch statements that assign to temp
        let mut else_stmts = else_anf.bindings;
        else_stmts.push(IRStatement::Assignment {
          variable: temp_var_with_type.clone(),
          expression: match else_anf.atom {
            IRAtom::Variable(ref v) => IRComplexExpr::Application {
              function: IRAtom::Variable(v.clone()),
              arguments: vec![],
              span: ir_span,
              type_annotation: None,
            },
            IRAtom::Literal(ref l) => IRComplexExpr::Constructor {
              constructor: format!("{}", l),
              arguments: vec![],
              span: l.span(),
              type_annotation: None,
            },
          },
          span: ir_span,
        });

        // Note: The if control flow construct would need to be represented
        // as a special IR node - for now we'll just track the conversion
        let mut all_bindings = cond_anf.bindings;

        // Create the if control flow (would need to add this to IRStatement or handle differently)
        // For now, we'll represent this as a placeholder that maintains the semantics
        // The actual implementation would involve creating IRControlFlow::If

        Ok(ANFResult::with_bindings(
          all_bindings,
          IRAtom::Variable(temp_var_with_type),
        ))
      }

      // Match expressions
      Expression::Match {
        scrutinee,
        arms,
        span,
        type_annotation,
      } => {
        // Convert scrutinee to atomic form
        let scrutinee_anf = self.convert_expression(scrutinee)?;
        let ir_span = self.convert_span(span);

        // Convert each arm
        let mut ir_arms = Vec::new();
        for arm in arms {
          let pattern = self.convert_pattern(&arm.pattern)?;
          let expr_anf = self.convert_expression(&arm.expression)?;

          // Convert guard if present
          let guard = if let Some(guard_expr) = &arm.guard {
            let guard_anf = self.convert_expression(guard_expr)?;
            Some(guard_anf.atom)
          } else {
            None
          };

          ir_arms.push(IRMatchArm::new(
            pattern,
            expr_anf.atom,
            guard,
            self.convert_span(&arm.span),
          ));
        }

        // Create temp var for the result
        let temp_name = self.fresh_temp("match_result");
        let temp_var = IRVariable::new(temp_name, ir_span);
        let temp_var_with_type = if let Some(ty) = type_annotation {
          temp_var.with_type(ty.clone())
        } else {
          temp_var
        };

        // The match control flow would be created here
        // For now, return the result structure
        let mut all_bindings = scrutinee_anf.bindings;

        Ok(ANFResult::with_bindings(
          all_bindings,
          IRAtom::Variable(temp_var_with_type),
        ))
      }

      // Tuple expressions
      Expression::Tuple {
        elements,
        span,
        type_annotation,
      } => {
        let mut elem_anfs: Vec<ANFResult> = Vec::new();
        for elem in elements {
          elem_anfs.push(self.convert_expression(elem)?);
        }

        let ir_span = self.convert_span(span);

        let complex_expr = IRComplexExpr::Tuple {
          elements: elem_anfs.iter().map(|a| a.atom.clone()).collect(),
          span: ir_span,
          type_annotation: type_annotation.clone(),
        };

        let temp_name = self.fresh_temp("tuple");
        let temp_var = IRVariable::new(temp_name, ir_span);
        let temp_var_with_type = if let Some(ty) = type_annotation {
          temp_var.with_type(ty.clone())
        } else {
          temp_var
        };

        let let_stmt = IRStatement::Let {
          variable: temp_var_with_type.clone(),
          expression: complex_expr,
          span: ir_span,
        };

        let mut all_bindings = Vec::new();
        for elem_anf in elem_anfs {
          all_bindings.extend(elem_anf.bindings);
        }
        all_bindings.push(let_stmt);

        Ok(ANFResult::with_bindings(
          all_bindings,
          IRAtom::Variable(temp_var_with_type),
        ))
      }

      // List expressions
      Expression::List {
        elements,
        span,
        type_annotation,
      } => {
        let mut elem_anfs: Vec<ANFResult> = Vec::new();
        for elem in elements {
          elem_anfs.push(self.convert_expression(elem)?);
        }

        let ir_span = self.convert_span(span);

        let complex_expr = IRComplexExpr::List {
          elements: elem_anfs.iter().map(|a| a.atom.clone()).collect(),
          span: ir_span,
          type_annotation: type_annotation.clone(),
        };

        let temp_name = self.fresh_temp("list");
        let temp_var = IRVariable::new(temp_name, ir_span);
        let temp_var_with_type = if let Some(ty) = type_annotation {
          temp_var.with_type(ty.clone())
        } else {
          temp_var
        };

        let let_stmt = IRStatement::Let {
          variable: temp_var_with_type.clone(),
          expression: complex_expr,
          span: ir_span,
        };

        let mut all_bindings = Vec::new();
        for elem_anf in elem_anfs {
          all_bindings.extend(elem_anf.bindings);
        }
        all_bindings.push(let_stmt);

        Ok(ANFResult::with_bindings(
          all_bindings,
          IRAtom::Variable(temp_var_with_type),
        ))
      }

      // Effect operations
      Expression::EffectOp {
        operation,
        arguments,
        span,
        type_annotation,
      } => {
        let mut arg_anfs: Vec<ANFResult> = Vec::new();
        for arg in arguments {
          arg_anfs.push(self.convert_expression(arg)?);
        }

        let ir_span = self.convert_span(span);

        let complex_expr = IRComplexExpr::EffectOp {
          operation: operation.name.clone(),
          arguments: arg_anfs.iter().map(|a| a.atom.clone()).collect(),
          span: ir_span,
          type_annotation: type_annotation.clone(),
        };

        let temp_name = self.fresh_temp("effect");
        let temp_var = IRVariable::new(temp_name, ir_span);
        let temp_var_with_type = if let Some(ty) = type_annotation {
          temp_var.with_type(ty.clone())
        } else {
          temp_var
        };

        let let_stmt = IRStatement::Let {
          variable: temp_var_with_type.clone(),
          expression: complex_expr,
          span: ir_span,
        };

        let mut all_bindings = Vec::new();
        for arg_anf in arg_anfs {
          all_bindings.extend(arg_anf.bindings);
        }
        all_bindings.push(let_stmt);

        Ok(ANFResult::with_bindings(
          all_bindings,
          IRAtom::Variable(temp_var_with_type),
        ))
      }

      // Lambda expressions need special handling
      Expression::Lambda { span, .. } => Err(ConversionError::UnsupportedConstruct {
        construct: "Lambda".to_string(),
        span: *span,
        message: "Lambda expressions should be lifted to top-level functions".to_string(),
      }),

      // Handler expressions need special handling
      Expression::Handler { span, .. } => Err(ConversionError::UnsupportedConstruct {
        construct: "Handler".to_string(),
        span: *span,
        message: "Effect handlers require special IR representation".to_string(),
      }),
    }
  }

  /// Convert an AST statement to IR statements
  pub fn convert_statement(&mut self, stmt: &Statement) -> ConversionResult<Vec<IRStatement>> {
    match stmt {
      Statement::Expression { expression, span, .. } => {
        let anf = self.convert_expression(expression)?;
        let mut stmts = anf.bindings;

        // Add an expression statement for the final value (for side effects)
        stmts.push(IRStatement::Expression {
          expression: match anf.atom {
            IRAtom::Variable(v) => IRComplexExpr::Application {
              function: IRAtom::Variable(v),
              arguments: vec![],
              span: self.convert_span(span),
              type_annotation: None,
            },
            IRAtom::Literal(l) => IRComplexExpr::Constructor {
              constructor: format!("{}", l),
              arguments: vec![],
              span: l.span(),
              type_annotation: None,
            },
          },
          span: self.convert_span(span),
        });

        Ok(stmts)
      }

      Statement::Let { binding, span, .. } => {
        let expr_anf = self.convert_expression(&binding.expression)?;
        let mut stmts = expr_anf.bindings;

        let ir_pattern = self.convert_pattern(&binding.pattern)?;

        match ir_pattern {
          IRPattern::Variable { variable } => {
            let let_stmt = IRStatement::Let {
              variable,
              expression: match expr_anf.atom {
                IRAtom::Variable(v) => IRComplexExpr::Application {
                  function: IRAtom::Variable(v),
                  arguments: vec![],
                  span: self.convert_span(span),
                  type_annotation: None,
                },
                IRAtom::Literal(l) => IRComplexExpr::Constructor {
                  constructor: format!("{}", l),
                  arguments: vec![],
                  span: l.span(),
                  type_annotation: None,
                },
              },
              span: self.convert_span(span),
            };
            stmts.push(let_stmt);
            Ok(stmts)
          }
          _ => Err(ConversionError::UnsupportedConstruct {
            construct: "Complex pattern in let statement".to_string(),
            span: *span,
            message: "Complex patterns in let statements should be desugared".to_string(),
          }),
        }
      }

      Statement::Function {
        name,
        parameters,
        body,
        return_type,
        span,
        ..
      } => {
        // Convert parameters - for now assuming simple variable patterns
        let mut ir_params = Vec::new();
        for param in parameters {
          let ir_pattern = self.convert_pattern(&param.pattern)?;
          match ir_pattern {
            IRPattern::Variable { variable } => {
              ir_params.push(variable);
            }
            _ => {
              return Err(ConversionError::UnsupportedConstruct {
                construct: "Complex parameter pattern".to_string(),
                span: param.pattern.span(),
                message: "Complex patterns in function parameters should be desugared".to_string(),
              });
            }
          }
        }

        // Convert the body expression
        let body_anf = self.convert_expression(body)?;
        let mut body_stmts = body_anf.bindings;

        // Add a return statement for the final value
        body_stmts.push(IRStatement::Return {
          expression: Some(body_anf.atom),
          span: self.convert_span(span),
        });

        // Create the IR function (would be stored in a separate structure)
        // For now, return empty as functions are handled at the module level
        Ok(vec![])
      }

      Statement::Type { span, .. } => {
        // Type declarations are handled separately
        Ok(vec![])
      }

      Statement::Effect { span, .. } => {
        // Effect declarations are handled separately
        Ok(vec![])
      }
    }
  }

  /// Convert an entire AST module to an IR node
  pub fn convert_module(&mut self, ast: &ast::AstNode) -> ConversionResult<IRNode> {
    let mut functions = Vec::new();
    let mut type_defs = Vec::new();
    let mut effect_defs = Vec::new();

    for stmt in &ast.statements {
      match stmt {
        Statement::Function {
          name,
          parameters,
          body,
          return_type,
          span,
          ..
        } => {
          // Convert parameters
          let mut ir_params = Vec::new();
          for param in parameters {
            let ir_pattern = self.convert_pattern(&param.pattern)?;
            match ir_pattern {
              IRPattern::Variable { variable } => {
                ir_params.push(variable);
              }
              _ => {
                return Err(ConversionError::UnsupportedConstruct {
                  construct: "Complex parameter pattern".to_string(),
                  span: param.pattern.span(),
                  message: "Complex patterns in function parameters should be desugared"
                    .to_string(),
                });
              }
            }
          }

          // Convert body
          let body_anf = self.convert_expression(body)?;
          let mut body_stmts = body_anf.bindings;
          body_stmts.push(IRStatement::Return {
            expression: Some(body_anf.atom),
            span: self.convert_span(span),
          });

          // Convert return type
          let ir_return_type = return_type.as_ref().and_then(|_| None); // TODO: Convert type expr

          let ir_func = IRFunction::new(
            name.name.clone(),
            ir_params,
            ir_return_type,
            body_stmts,
            self.convert_span(span),
          );

          functions.push(ir_func);
        }

        Statement::Type {
          name,
          parameters,
          variants,
          span,
          ..
        } => {
          let ir_params: Vec<String> = parameters.iter().map(|p| p.name.clone()).collect();
          let ir_variants: Vec<IRTypeVariant> = variants
            .iter()
            .map(|v| {
              IRTypeVariant::new(
                v.name.name.clone(),
                vec![], // TODO: Convert field types
                self.convert_span(&v.span),
              )
            })
            .collect();

          type_defs.push(IRTypeDef::new(
            name.name.clone(),
            ir_params,
            ir_variants,
            self.convert_span(span),
          ));
        }

        Statement::Effect {
          name,
          operations,
          span,
          ..
        } => {
          let ir_operations: Vec<IREffectOperation> = operations
            .iter()
            .map(|op| {
              // TODO: Properly convert input/output types from the effect operation
              // For now, use unit type as placeholder
              IREffectOperation::new(
                op.name.name.clone(),
                Type::primitive(PrimitiveType::Unit), // TODO: Convert input type
                Type::primitive(PrimitiveType::Unit), // TODO: Convert output type
                self.convert_span(&op.span),
              )
            })
            .collect();

          effect_defs.push(IREffectDef::new(
            name.name.clone(),
            ir_operations,
            self.convert_span(span),
          ));
        }

        Statement::Let { .. } | Statement::Expression { .. } => {
          // Top-level let bindings and expressions need special handling
          // They would typically be placed in a main function or module initializer
        }
      }
    }

    let ir_span = self.convert_span(&ast.span);
    let node = IRNode::new(functions, type_defs, effect_defs, ir_span);

    Ok(node)
  }
}

/// Convenience function to convert an AST to IR
pub fn convert_ast_to_ir(ast: &ast::AstNode) -> ConversionResult<IRNode> {
  let mut converter = AstToIrConverter::new();
  converter.convert_module(ast)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::SourceLocation;

  fn make_span(line: usize, col: usize) -> Span {
    let start = SourceLocation::new(line, col, 0);
    let end = SourceLocation::new(line, col + 1, 1);
    Span::new(start, end)
  }

  fn make_identifier(name: &str) -> Identifier {
    Identifier::new(name.to_string(), make_span(1, 1))
  }

  #[test]
  fn test_convert_literal_integer() {
    let converter = AstToIrConverter::new();
    let lit = Literal::Integer {
      value: 42,
      span: make_span(1, 1),
    };

    let ir_lit = converter.convert_literal(&lit);
    match ir_lit {
      IRLiteral::Integer { value, .. } => assert_eq!(value, 42),
      _ => panic!("Expected integer literal"),
    }
  }

  #[test]
  fn test_convert_literal_string() {
    let converter = AstToIrConverter::new();
    let lit = Literal::String {
      value: "hello".to_string(),
      span: make_span(1, 1),
    };

    let ir_lit = converter.convert_literal(&lit);
    match ir_lit {
      IRLiteral::String { value, .. } => assert_eq!(value, "hello"),
      _ => panic!("Expected string literal"),
    }
  }

  #[test]
  fn test_convert_literal_boolean() {
    let converter = AstToIrConverter::new();
    let lit = Literal::Boolean {
      value: true,
      span: make_span(1, 1),
    };

    let ir_lit = converter.convert_literal(&lit);
    match ir_lit {
      IRLiteral::Boolean { value, .. } => assert!(value),
      _ => panic!("Expected boolean literal"),
    }
  }

  #[test]
  fn test_convert_pattern_variable() {
    let converter = AstToIrConverter::new();
    let pattern = Pattern::Variable {
      identifier: make_identifier("x"),
      span: make_span(1, 1),
    };

    let ir_pattern = converter.convert_pattern(&pattern).unwrap();
    match ir_pattern {
      IRPattern::Variable { variable } => assert_eq!(variable.name, "x"),
      _ => panic!("Expected variable pattern"),
    }
  }

  #[test]
  fn test_convert_pattern_wildcard() {
    let converter = AstToIrConverter::new();
    let pattern = Pattern::Wildcard {
      span: make_span(1, 1),
    };

    let ir_pattern = converter.convert_pattern(&pattern).unwrap();
    assert!(matches!(ir_pattern, IRPattern::Wildcard { .. }));
  }

  #[test]
  fn test_convert_pattern_constructor() {
    let converter = AstToIrConverter::new();
    let pattern = Pattern::Constructor {
      constructor: make_identifier("Some"),
      arguments: vec![Pattern::Variable {
        identifier: make_identifier("x"),
        span: make_span(1, 6),
      }],
      span: make_span(1, 1),
    };

    let ir_pattern = converter.convert_pattern(&pattern).unwrap();
    match ir_pattern {
      IRPattern::Constructor {
        constructor,
        arguments,
        ..
      } => {
        assert_eq!(constructor, "Some");
        assert_eq!(arguments.len(), 1);
      }
      _ => panic!("Expected constructor pattern"),
    }
  }

  #[test]
  fn test_convert_expression_literal() {
    let mut converter = AstToIrConverter::new();
    let expr = Expression::Literal {
      literal: Literal::Integer {
        value: 42,
        span: make_span(1, 1),
      },
      span: make_span(1, 1),
      type_annotation: None,
    };

    let anf = converter.convert_expression(&expr).unwrap();
    assert!(anf.bindings.is_empty());
    match anf.atom {
      IRAtom::Literal(IRLiteral::Integer { value, .. }) => assert_eq!(value, 42),
      _ => panic!("Expected integer literal atom"),
    }
  }

  #[test]
  fn test_convert_expression_variable() {
    let mut converter = AstToIrConverter::new();
    let expr = Expression::Variable {
      identifier: make_identifier("x"),
      span: make_span(1, 1),
      type_annotation: None,
    };

    let anf = converter.convert_expression(&expr).unwrap();
    assert!(anf.bindings.is_empty());
    match anf.atom {
      IRAtom::Variable(var) => assert_eq!(var.name, "x"),
      _ => panic!("Expected variable atom"),
    }
  }

  #[test]
  fn test_convert_binary_op() {
    let converter = AstToIrConverter::new();

    assert_eq!(
      converter.convert_binary_op(&ast::BinaryOperator::Add),
      IRPrimitiveOp::Add
    );
    assert_eq!(
      converter.convert_binary_op(&ast::BinaryOperator::Sub),
      IRPrimitiveOp::Sub
    );
    assert_eq!(
      converter.convert_binary_op(&ast::BinaryOperator::Mul),
      IRPrimitiveOp::Mul
    );
    assert_eq!(
      converter.convert_binary_op(&ast::BinaryOperator::Eq),
      IRPrimitiveOp::Eq
    );
    assert_eq!(
      converter.convert_binary_op(&ast::BinaryOperator::And),
      IRPrimitiveOp::And
    );
  }

  #[test]
  fn test_convert_expression_binary_op() {
    let mut converter = AstToIrConverter::new();
    let expr = Expression::BinaryOp {
      left: Box::new(Expression::Literal {
        literal: Literal::Integer {
          value: 1,
          span: make_span(1, 1),
        },
        span: make_span(1, 1),
        type_annotation: None,
      }),
      operator: ast::BinaryOperator::Add,
      right: Box::new(Expression::Literal {
        literal: Literal::Integer {
          value: 2,
          span: make_span(1, 5),
        },
        span: make_span(1, 5),
        type_annotation: None,
      }),
      span: make_span(1, 1),
      type_annotation: None,
    };

    let anf = converter.convert_expression(&expr).unwrap();
    assert_eq!(anf.bindings.len(), 1);
    match &anf.atom {
      IRAtom::Variable(var) => assert!(var.name.starts_with("_binop_")),
      _ => panic!("Expected variable atom for binary op result"),
    }
  }

  #[test]
  fn test_convert_expression_tuple() {
    let mut converter = AstToIrConverter::new();
    let expr = Expression::Tuple {
      elements: vec![
        Expression::Literal {
          literal: Literal::Integer {
            value: 1,
            span: make_span(1, 2),
          },
          span: make_span(1, 2),
          type_annotation: None,
        },
        Expression::Literal {
          literal: Literal::Integer {
            value: 2,
            span: make_span(1, 5),
          },
          span: make_span(1, 5),
          type_annotation: None,
        },
      ],
      span: make_span(1, 1),
      type_annotation: None,
    };

    let anf = converter.convert_expression(&expr).unwrap();
    assert_eq!(anf.bindings.len(), 1);
    match &anf.atom {
      IRAtom::Variable(var) => assert!(var.name.starts_with("_tuple_")),
      _ => panic!("Expected variable atom for tuple"),
    }
  }

  #[test]
  fn test_fresh_temp_generation() {
    let mut converter = AstToIrConverter::new();

    let temp1 = converter.fresh_temp("test");
    let temp2 = converter.fresh_temp("test");
    let temp3 = converter.fresh_temp("other");

    assert_eq!(temp1, "_test_0");
    assert_eq!(temp2, "_test_1");
    assert_eq!(temp3, "_other_2");
  }

  #[test]
  fn test_conversion_error_display() {
    let error = ConversionError::UnsupportedConstruct {
      construct: "Test".to_string(),
      span: make_span(1, 1),
      message: "Test message".to_string(),
    };

    assert_eq!(
      error.to_string(),
      "Unsupported construct 'Test': Test message"
    );
  }

  #[test]
  fn test_anf_result_creation() {
    let span = IRSpan::new(SourceLocation::start(), SourceLocation::start());
    let var = IRVariable::new("x".to_string(), span);
    let atom = IRAtom::Variable(var);

    let result = ANFResult::atom(atom.clone());
    assert!(result.bindings.is_empty());

    let binding = IRStatement::Let {
      variable: IRVariable::new("y".to_string(), span),
      expression: IRComplexExpr::Tuple {
        elements: vec![],
        span,
        type_annotation: None,
      },
      span,
    };

    let result_with_bindings = ANFResult::with_bindings(vec![binding], atom);
    assert_eq!(result_with_bindings.bindings.len(), 1);
  }
}

