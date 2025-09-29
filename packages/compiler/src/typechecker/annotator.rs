//! Type annotator for the Lattice language.
//!
//! This module provides functionality to annotate AST nodes with
//! inferred type information using the Hindley-Milner type inference system.

use crate::types::inference::TypeInference;
use crate::types::types::{Type, PrimitiveType, ProductType, SumType};
use std::collections::HashMap;

use super::errors::TypeCheckError;

/// The main type annotator that performs type inference and annotation
#[derive(Debug)]
pub struct TypeAnnotator {
  /// Type inference engine
  inference: TypeInference,
  /// Error collection
  errors: Vec<TypeCheckError>,
  /// Type environment for storing type definitions
  type_env: HashMap<String, Type>,
  /// Variable environment for storing variable types
  var_env: HashMap<String, Type>,
}

/// Represents a simple expression for type checking demonstration
#[derive(Debug, Clone)]
pub enum SimpleExpression {
  /// Literal value
  Literal(Type),
  /// Variable reference
  Variable(String),
  /// Function application
  Application { function: String, arguments: Vec<SimpleExpression> },
  /// Match expression
  Match { scrutinee: Box<SimpleExpression>, arms: Vec<SimpleMatchArm> },
}

/// Represents a simple match arm
#[derive(Debug, Clone)]
pub struct SimpleMatchArm {
  /// Pattern (simplified to just constructor name)
  pub pattern: String,
  /// Expression to evaluate
  pub expression: SimpleExpression,
}

/// Represents a simple pattern for type checking
#[derive(Debug, Clone)]
pub enum SimplePattern {
  /// Variable pattern
  Variable(String),
  /// Constructor pattern
  Constructor { name: String, arguments: Vec<SimplePattern> },
  /// Wildcard pattern
  Wildcard,
}

impl TypeAnnotator {
  /// Create a new type annotator
  pub fn new() -> Self {
    Self {
      inference: TypeInference::new(),
      errors: Vec::new(),
      type_env: HashMap::new(),
      var_env: HashMap::new(),
    }
  }

  /// Add a type definition to the environment
  pub fn add_type_definition(&mut self, name: String, ty: Type) {
    self.type_env.insert(name, ty);
  }

  /// Add a variable binding to the environment
  pub fn add_variable_binding(&mut self, name: String, ty: Type) {
    self.var_env.insert(name, ty);
  }

  /// Type check a simple expression
  pub fn type_check_expression(&mut self, expr: &SimpleExpression) -> Result<Type, TypeCheckError> {
    match expr {
      SimpleExpression::Literal(ty) => Ok(ty.clone()),
      SimpleExpression::Variable(name) => {
        if let Some(ty) = self.var_env.get(name) {
          Ok(ty.clone())
        } else {
          Err(TypeCheckError::UndefinedVariable {
            name: name.clone(),
            location: None,
          })
        }
      }
      SimpleExpression::Application { function, arguments } => {
        self.type_check_application(function, arguments)
      }
      SimpleExpression::Match { scrutinee, arms } => {
        self.type_check_match_expression(scrutinee, arms)
      }
    }
  }

  /// Type check a function application
  fn type_check_application(
    &mut self,
    function: &str,
    arguments: &[SimpleExpression],
  ) -> Result<Type, TypeCheckError> {
    // Get function type from environment
    let func_type = if let Some(ty) = self.var_env.get(function) {
      ty.clone()
    } else {
      return Err(TypeCheckError::UndefinedVariable {
        name: function.to_string(),
        location: None,
      });
    };

    // Type check arguments
    let mut arg_types = Vec::new();
    for arg in arguments {
      arg_types.push(self.type_check_expression(arg)?);
    }

    // Create expected function type
    let return_type = self.inference.fresh_type_variable();
    let expected_func_type = Type::function(arg_types, return_type.clone());

    // Unify with actual function type
    let subst = self.inference.unify(&func_type, &expected_func_type)
      .map_err(|e| TypeCheckError::TypeMismatch {
        expected: expected_func_type,
        actual: func_type,
        location: None,
        context: None,
      })?;

    let final_return_type = subst.apply(&return_type);
    Ok(final_return_type)
  }

  /// Type check a match expression
  fn type_check_match_expression(
    &mut self,
    scrutinee: &SimpleExpression,
    arms: &[SimpleMatchArm],
  ) -> Result<Type, TypeCheckError> {
    let scrutinee_type = self.type_check_expression(scrutinee)?;

    if arms.is_empty() {
      return Err(TypeCheckError::EmptyMatchExpression {
        location: None,
      });
    }

    // Type check all arms
    let mut arm_types = Vec::new();
    for arm in arms.iter() {
      // Check pattern compatibility
      self.type_check_simple_pattern(&arm.pattern, &scrutinee_type)?;

      // Type check the expression
      let arm_type = self.type_check_expression(&arm.expression)?;
      arm_types.push(arm_type);
    }

    // All arms must have the same type
    if arm_types.len() < 2 {
      return Ok(arm_types[0].clone());
    }

    let mut unified_type = arm_types[0].clone();
    for arm_type in arm_types.iter().skip(1) {
      let subst = self.inference.unify(&unified_type, arm_type)
        .map_err(|e| TypeCheckError::TypeMismatch {
          expected: unified_type.clone(),
          actual: arm_type.clone(),
          location: None,
          context: None,
        })?;
      unified_type = subst.apply(&unified_type);
    }

    Ok(unified_type)
  }

  /// Type check a simple pattern
  fn type_check_simple_pattern(
    &mut self,
    pattern: &str,
    expected_type: &Type,
  ) -> Result<(), TypeCheckError> {
    // Check if pattern is a constructor for the expected type
    if let Type::Sum(sum_type) = expected_type {
      if sum_type.variants.iter().any(|(name, _)| name == pattern) {
        Ok(())
      } else {
        Err(TypeCheckError::UndefinedConstructor {
          constructor_name: pattern.to_string(),
          type_name: "Unknown".to_string(),
          location: None,
        })
      }
    } else {
      Err(TypeCheckError::TypeMismatch {
        expected: Type::Sum(SumType::new(vec![])),
        actual: expected_type.clone(),
        location: None,
        context: None,
      })
    }
  }

  /// Get the collected errors
  pub fn errors(&self) -> &[TypeCheckError] {
    &self.errors
  }

  /// Clear collected errors
  pub fn clear_errors(&mut self) {
    self.errors.clear();
  }

  /// Get the type inference engine
  pub fn inference(&self) -> &TypeInference {
    &self.inference
  }

  /// Get the type inference engine mutably
  pub fn inference_mut(&mut self) -> &mut TypeInference {
    &mut self.inference
  }

  /// Type check a declaration
  fn type_check_declaration(&mut self, decl: &mut Declaration) -> Result<(), TypeCheckError> {
    match decl {
      Declaration::Let { bindings, .. } => {
        for binding in bindings.iter_mut() {
          self.type_check_binding(binding)?;
        }
        Ok(())
      }
      Declaration::Function { name, parameters, return_type, body, .. } => {
        self.type_check_function(name, parameters, return_type, body)
      }
      _ => Ok(()), // Type declarations are handled in collect_type_definitions
    }
  }

  /// Get the collected errors
  pub fn errors(&self) -> &[TypeCheckError] {
    &self.errors
  }

  /// Clear collected errors
  pub fn clear_errors(&mut self) {
    self.errors.clear();
  }

  /// Get the type inference engine
  pub fn inference(&self) -> &TypeInference {
    &self.inference
  }

  /// Get the type inference engine mutably
  pub fn inference_mut(&mut self) -> &mut TypeInference {
    &mut self.inference
  }

  /// Convert a TypeExpr to a Type
  fn type_expr_to_type(&mut self, type_expr: &TypeExpr) -> Result<Type, TypeCheckError> {
    match type_expr {
      TypeExpr::Variable { name, .. } => {
        Ok(Type::named_var(self.inference.next_var_id(), name.clone()))
      }
      TypeExpr::Constructor { name, .. } => {
        if let Some(ty) = self.type_env.get(&name.name) {
          Ok(ty.clone())
        } else {
          // Check if it's a primitive type
          match name.name.as_str() {
            "Int" => Ok(Type::primitive(PrimitiveType::Int)),
            "Float" => Ok(Type::primitive(PrimitiveType::Float)),
            "Bool" => Ok(Type::primitive(PrimitiveType::Bool)),
            "String" => Ok(Type::primitive(PrimitiveType::String)),
            "Char" => Ok(Type::primitive(PrimitiveType::Char)),
            "Unit" => Ok(Type::primitive(PrimitiveType::Unit)),
            _ => Err(TypeCheckError::UndefinedType {
              type_name: name.name.clone(),
              location: Some(name.span.start),
            }),
          }
        }
      }
      TypeExpr::Function { parameter, return_type, .. } => {
        let param_type = self.type_expr_to_type(parameter)?;
        let return_type = self.type_expr_to_type(return_type)?;
        Ok(Type::arrow(param_type, return_type))
      }
      TypeExpr::Tuple { elements, .. } => {
        let mut field_types = Vec::new();
        for elem in elements {
          field_types.push(self.type_expr_to_type(elem)?);
        }
        Ok(Type::tuple(field_types))
      }
      TypeExpr::List { element_type, .. } => {
        let elem_type = self.type_expr_to_type(element_type)?;
        Ok(Type::application("List".to_string(), vec![elem_type]))
      }
      TypeExpr::Generic { constructor, arguments, .. } => {
        let mut arg_types = Vec::new();
        for arg in arguments {
          arg_types.push(self.type_expr_to_type(arg)?);
        }
        Ok(Type::application(constructor.name.clone(), arg_types))
      }
      TypeExpr::Effect { input_type, output_type, .. } => {
        let input_ty = self.type_expr_to_type(input_type)?;
        let output_ty = self.type_expr_to_type(output_type)?;
        Ok(Type::effect("Effect".to_string(), vec![input_ty, output_ty]))
      }
    }
  }

  /// Create a sum type from variants
  fn create_sum_type(&mut self, variants: &[(String, Option<TypeExpr>)]) -> Result<Type, TypeCheckError> {
    let mut variant_types = Vec::new();
    for (name, type_expr) in variants {
      let variant_type = if let Some(expr) = type_expr {
        Some(self.type_expr_to_type(expr)?)
      } else {
        None
      };
      variant_types.push((name.clone(), variant_type));
    }
    Ok(Type::sum(variant_types))
  }

  /// Create a product type from fields
  fn create_product_type(&mut self, fields: &[(Option<String>, TypeExpr)]) -> Result<Type, TypeCheckError> {
    let mut field_types = Vec::new();
    for (name, type_expr) in fields {
      let field_type = self.type_expr_to_type(type_expr)?;
      field_types.push((name.clone(), field_type));
    }
    Ok(Type::Product(ProductType::new(field_types)))
  }

  /// Type check a binding
  fn type_check_binding(&mut self, binding: &mut Binding) -> Result<(), TypeCheckError> {
    let expr_type = self.type_check_expression(&mut binding.expression)?;
    
    // If there's a type annotation, check that it matches
    if let Some(annotation) = &binding.type_annotation {
      let expected_type = self.type_expr_to_type(annotation)?;
      let subst = self.inference.unify(&expr_type, &expected_type)
        .map_err(|e| TypeCheckError::TypeMismatch {
          expected: expected_type,
          actual: expr_type,
          location: Some(binding.span.start),
          context: None,
        })?;
      
      // Apply substitution
      let final_type = subst.apply(&expr_type);
      binding.expression.set_type_annotation(final_type.clone());
      
      // Store variable type based on pattern
      self.store_pattern_binding(&binding.pattern, &final_type);
    } else {
      // No annotation, infer the type
      binding.expression.set_type_annotation(expr_type.clone());
      self.store_pattern_binding(&binding.pattern, &expr_type);
    }
    
    Ok(())
  }

  /// Store variable bindings from a pattern
  fn store_pattern_binding(&mut self, pattern: &Pattern, ty: &Type) {
    match pattern {
      Pattern::Variable { identifier, .. } => {
        self.var_env.insert(identifier.name.clone(), ty.clone());
      }
      Pattern::As { identifier, .. } => {
        self.var_env.insert(identifier.name.clone(), ty.clone());
      }
      _ => {
        // Other patterns don't bind variables at the top level
      }
    }
  }

  /// Type check a function
  fn type_check_function(
    &mut self,
    name: &Identifier,
    parameters: &[(Identifier, Option<TypeExpr>)],
    return_type: &Option<TypeExpr>,
    body: &mut Expression,
  ) -> Result<(), TypeCheckError> {
    // Create parameter types
    let mut param_types = Vec::new();
    for (param_name, param_type_expr) in parameters {
      let param_type = if let Some(type_expr) = param_type_expr {
        self.type_expr_to_type(type_expr)?
      } else {
        self.inference.fresh_type_variable()
      };
      param_types.push(param_type.clone());
      self.var_env.insert(param_name.name.clone(), param_type);
    }

    // Type check the body
    let body_type = self.type_check_expression(body)?;

    // Check return type if specified
    let final_return_type = if let Some(return_type_expr) = return_type {
      let expected_return_type = self.type_expr_to_type(return_type_expr)?;
      let subst = self.inference.unify(&body_type, &expected_return_type)
        .map_err(|e| TypeCheckError::TypeMismatch {
          expected: expected_return_type.clone(),
          actual: body_type,
          location: Some(name.span.start),
          context: None,
        })?;
      subst.apply(&expected_return_type)
    } else {
      body_type
    };

    // Create function type
    let function_type = Type::function(param_types, final_return_type);
    self.var_env.insert(name.name.clone(), function_type);

    Ok(())
  }

  /// Type check an expression
  fn type_check_expression(&mut self, expr: &mut Expression) -> Result<Type, TypeCheckError> {
    match expr {
      Expression::Literal { literal, .. } => {
        let ty = match literal {
          Literal::Integer { .. } => Type::primitive(PrimitiveType::Int),
          Literal::Float { .. } => Type::primitive(PrimitiveType::Float),
          Literal::String { .. } => Type::primitive(PrimitiveType::String),
          Literal::Char { .. } => Type::primitive(PrimitiveType::Char),
          Literal::Boolean { .. } => Type::primitive(PrimitiveType::Bool),
        };
        expr.set_type_annotation(ty.clone());
        Ok(ty)
      }
      Expression::Variable { identifier, .. } => {
        if let Some(ty) = self.var_env.get(&identifier.name) {
          expr.set_type_annotation(ty.clone());
          Ok(ty.clone())
        } else {
          Err(TypeCheckError::UndefinedVariable {
            name: identifier.name.clone(),
            location: Some(identifier.span.start),
          })
        }
      }
      Expression::Application { function, arguments, .. } => {
        let func_type = self.type_check_expression(function)?;
        let mut arg_types = Vec::new();
        
        for arg in arguments.iter_mut() {
          arg_types.push(self.type_check_expression(arg)?);
        }

        // Create expected function type
        let return_type = self.inference.fresh_type_variable();
        let expected_func_type = Type::function(arg_types, return_type.clone());

        // Unify with actual function type
        let subst = self.inference.unify(&func_type, &expected_func_type)
          .map_err(|e| TypeCheckError::TypeMismatch {
            expected: expected_func_type,
            actual: func_type,
            location: Some(expr.span().start),
            context: None,
          })?;

        let final_return_type = subst.apply(&return_type);
        expr.set_type_annotation(final_return_type.clone());
        Ok(final_return_type)
      }
      Expression::Match { scrutinee, arms, .. } => {
        self.type_check_match_expression(scrutinee, arms, expr)
      }
      Expression::Lambda { parameters, body, .. } => {
        self.type_check_lambda_expression(parameters, body, expr)
      }
      Expression::Let { bindings, body, .. } => {
        // Create new scope for bindings
        let old_var_env = self.var_env.clone();
        
        for binding in bindings.iter_mut() {
          self.type_check_binding(binding)?;
        }
        
        let body_type = self.type_check_expression(body)?;
        
        // Restore old environment
        self.var_env = old_var_env;
        
        expr.set_type_annotation(body_type.clone());
        Ok(body_type)
      }
      Expression::If { condition, then_branch, else_branch, .. } => {
        let cond_type = self.type_check_expression(condition)?;
        let bool_type = Type::primitive(PrimitiveType::Bool);
        
        // Check condition is boolean
        self.inference.unify(&cond_type, &bool_type)
          .map_err(|e| TypeCheckError::TypeMismatch {
            expected: bool_type,
            actual: cond_type,
            location: Some(condition.span().start),
            context: None,
          })?;

        let then_type = self.type_check_expression(then_branch)?;
        let else_type = self.type_check_expression(else_branch)?;

        // Both branches must have the same type
        let subst = self.inference.unify(&then_type, &else_type)
          .map_err(|e| TypeCheckError::TypeMismatch {
            expected: then_type.clone(),
            actual: else_type,
            location: Some(else_branch.span().start),
            context: None,
          })?;

        let final_type = subst.apply(&then_type);
        expr.set_type_annotation(final_type.clone());
        Ok(final_type)
      }
      Expression::BinaryOp { left, operator, right, .. } => {
        self.type_check_binary_operation(left, operator, right, expr)
      }
      Expression::UnaryOp { operator, operand, .. } => {
        self.type_check_unary_operation(operator, operand, expr)
      }
    }
  }

  /// Type check a match expression
  fn type_check_match_expression(
    &mut self,
    scrutinee: &mut Expression,
    arms: &mut [MatchArm],
    expr: &mut Expression,
  ) -> Result<Type, TypeCheckError> {
    let scrutinee_type = self.type_check_expression(scrutinee)?;
    
    if arms.is_empty() {
      return Err(TypeCheckError::EmptyMatchExpression {
        location: Some(expr.span().start),
      });
    }

    // Type check all arms
    let mut arm_types = Vec::new();
    for arm in arms.iter_mut() {
      // Check pattern compatibility
      self.type_check_pattern(&arm.pattern, &scrutinee_type)?;
      
      // Type check the expression
      let arm_type = self.type_check_expression(&mut arm.expression)?;
      arm_types.push(arm_type);
    }

    // All arms must have the same type
    if arm_types.len() < 2 {
      let final_type = arm_types[0].clone();
      expr.set_type_annotation(final_type.clone());
      return Ok(final_type);
    }

    let mut unified_type = arm_types[0].clone();
    for (i, arm_type) in arm_types.iter().enumerate().skip(1) {
      let subst = self.inference.unify(&unified_type, arm_type)
        .map_err(|e| TypeCheckError::TypeMismatch {
          expected: unified_type.clone(),
          actual: arm_type.clone(),
          location: Some(arms[i].span.start),
          context: None,
        })?;
      unified_type = subst.apply(&unified_type);
    }

    expr.set_type_annotation(unified_type.clone());
    Ok(unified_type)
  }

  /// Type check a pattern
  fn type_check_pattern(&mut self, pattern: &Pattern, expected_type: &Type) -> Result<(), TypeCheckError> {
    match pattern {
      Pattern::Variable { identifier, .. } => {
        // Variable patterns match any type
        self.var_env.insert(identifier.name.clone(), expected_type.clone());
        Ok(())
      }
      Pattern::Wildcard { .. } => {
        // Wildcard patterns match any type
        Ok(())
      }
      Pattern::Literal { literal, .. } => {
        let literal_type = match literal {
          Literal::Integer { .. } => Type::primitive(PrimitiveType::Int),
          Literal::Float { .. } => Type::primitive(PrimitiveType::Float),
          Literal::String { .. } => Type::primitive(PrimitiveType::String),
          Literal::Char { .. } => Type::primitive(PrimitiveType::Char),
          Literal::Boolean { .. } => Type::primitive(PrimitiveType::Bool),
        };
        
        self.inference.unify(&literal_type, expected_type)
          .map_err(|e| TypeCheckError::TypeMismatch {
            expected: literal_type,
            actual: expected_type.clone(),
            location: Some(pattern.span().start),
            context: None,
          })?;
        Ok(())
      }
      Pattern::Constructor { constructor, arguments, .. } => {
        // Check if constructor exists in expected type
        if let Type::Sum(sum_type) = expected_type {
          if let Some((_, variant_type)) = sum_type.variants.iter().find(|(name, _)| name == &constructor.name) {
            if let Some(variant_ty) = variant_type {
              if arguments.is_empty() {
                return Err(TypeCheckError::TypeMismatch {
                  expected: variant_ty.clone(),
                  actual: Type::primitive(PrimitiveType::Unit),
                  location: Some(pattern.span().start),
                  context: None,
                });
              } else if arguments.len() == 1 {
                self.type_check_pattern(&arguments[0], variant_ty)?;
              } else {
                // Multiple arguments - check if it's a tuple type
                if let Type::Product(product_type) = variant_ty {
                  if product_type.fields.len() == arguments.len() {
                    for (arg, (_, field_type)) in arguments.iter().zip(product_type.fields.iter()) {
                      self.type_check_pattern(arg, field_type)?;
                    }
                  } else {
                    return Err(TypeCheckError::TypeMismatch {
                      expected: variant_ty.clone(),
                      actual: Type::primitive(PrimitiveType::Unit),
                      location: Some(pattern.span().start),
                      context: None,
                    });
                  }
                } else {
                  return Err(TypeCheckError::TypeMismatch {
                    expected: variant_ty.clone(),
                    actual: Type::primitive(PrimitiveType::Unit),
                    location: Some(pattern.span().start),
                    context: None,
                  });
                }
              }
            } else if !arguments.is_empty() {
              return Err(TypeCheckError::TypeMismatch {
                expected: Type::primitive(PrimitiveType::Unit),
                actual: Type::primitive(PrimitiveType::Unit),
                location: Some(pattern.span().start),
                context: None,
              });
            }
          } else {
            return Err(TypeCheckError::UndefinedConstructor {
              constructor_name: constructor.name.clone(),
              type_name: "Unknown".to_string(),
              location: Some(pattern.span().start),
            });
          }
        } else {
          return Err(TypeCheckError::TypeMismatch {
            expected: Type::Sum(SumType::new(vec![])),
            actual: expected_type.clone(),
            location: Some(pattern.span().start),
            context: None,
          });
        }
        Ok(())
      }
      Pattern::Tuple { elements, .. } => {
        if let Type::Product(product_type) = expected_type {
          if elements.len() == product_type.fields.len() {
            for (elem, (_, field_type)) in elements.iter().zip(product_type.fields.iter()) {
              self.type_check_pattern(elem, field_type)?;
            }
          } else {
            return Err(TypeCheckError::TypeMismatch {
              expected: expected_type.clone(),
              actual: Type::primitive(PrimitiveType::Unit),
              location: Some(pattern.span().start),
              context: None,
            });
          }
        } else {
          return Err(TypeCheckError::TypeMismatch {
            expected: Type::Product(ProductType::new(vec![])),
            actual: expected_type.clone(),
            location: Some(pattern.span().start),
            context: None,
          });
        }
        Ok(())
      }
      Pattern::Or { left, right, .. } => {
        // Both patterns must match the same type
        self.type_check_pattern(left, expected_type)?;
        self.type_check_pattern(right, expected_type)?;
        Ok(())
      }
      Pattern::As { pattern, identifier, .. } => {
        // Check the pattern matches the expected type
        self.type_check_pattern(pattern, expected_type)?;
        // Bind the identifier to the expected type
        self.var_env.insert(identifier.name.clone(), expected_type.clone());
        Ok(())
      }
      Pattern::List { elements, .. } => {
        // For now, treat list patterns as matching List<T> where T is inferred
        let elem_type = self.inference.fresh_type_variable();
        let list_type = Type::application("List".to_string(), vec![elem_type]);
        
        self.inference.unify(&list_type, expected_type)
          .map_err(|e| TypeCheckError::TypeMismatch {
            expected: list_type,
            actual: expected_type.clone(),
            location: Some(pattern.span().start),
            context: None,
          })?;

        // Type check each element pattern
        for elem in elements {
          self.type_check_pattern(elem, &elem_type)?;
        }
        Ok(())
      }
    }
  }

  /// Type check a lambda expression
  fn type_check_lambda_expression(
    &mut self,
    parameters: &[(Identifier, Option<TypeExpr>)],
    body: &mut Expression,
    expr: &mut Expression,
  ) -> Result<Type, TypeCheckError> {
    // Create parameter types
    let mut param_types = Vec::new();
    let old_var_env = self.var_env.clone();
    
    for (param_name, param_type_expr) in parameters {
      let param_type = if let Some(type_expr) = param_type_expr {
        self.type_expr_to_type(type_expr)?
      } else {
        self.inference.fresh_type_variable()
      };
      param_types.push(param_type.clone());
      self.var_env.insert(param_name.name.clone(), param_type);
    }

    // Type check the body
    let body_type = self.type_check_expression(body)?;
    
    // Restore old environment
    self.var_env = old_var_env;

    // Create function type
    let function_type = Type::function(param_types, body_type.clone());
    expr.set_type_annotation(function_type.clone());
    Ok(function_type)
  }

  /// Type check a binary operation
  fn type_check_binary_operation(
    &mut self,
    left: &mut Expression,
    operator: &crate::parser::ast::BinaryOperator,
    right: &mut Expression,
    expr: &mut Expression,
  ) -> Result<Type, TypeCheckError> {
    let left_type = self.type_check_expression(left)?;
    let right_type = self.type_check_expression(right)?;

    let result_type = match operator {
      BinaryOperator::Add
      | BinaryOperator::Subtract
      | BinaryOperator::Multiply
      | BinaryOperator::Divide
      | BinaryOperator::Modulo => {
        // Arithmetic operations
        let int_type = Type::primitive(PrimitiveType::Int);
        self.inference.unify(&left_type, &int_type)
          .map_err(|e| TypeCheckError::TypeMismatch {
            expected: int_type.clone(),
            actual: left_type,
            location: Some(left.span().start),
            context: None,
          })?;
        self.inference.unify(&right_type, &int_type)
          .map_err(|e| TypeCheckError::TypeMismatch {
            expected: int_type.clone(),
            actual: right_type,
            location: Some(right.span().start),
            context: None,
          })?;
        int_type
      }
      BinaryOperator::Equal
      | BinaryOperator::NotEqual
      | BinaryOperator::LessThan
      | BinaryOperator::LessThanOrEqual
      | BinaryOperator::GreaterThan
      | BinaryOperator::GreaterThanOrEqual => {
        // Comparison operations
        self.inference.unify(&left_type, &right_type)
          .map_err(|e| TypeCheckError::TypeMismatch {
            expected: left_type.clone(),
            actual: right_type,
            location: Some(right.span().start),
            context: None,
          })?;
        Type::primitive(PrimitiveType::Bool)
      }
      BinaryOperator::And
      | BinaryOperator::Or => {
        // Logical operations
        let bool_type = Type::primitive(PrimitiveType::Bool);
        self.inference.unify(&left_type, &bool_type)
          .map_err(|e| TypeCheckError::TypeMismatch {
            expected: bool_type.clone(),
            actual: left_type,
            location: Some(left.span().start),
            context: None,
          })?;
        self.inference.unify(&right_type, &bool_type)
          .map_err(|e| TypeCheckError::TypeMismatch {
            expected: bool_type.clone(),
            actual: right_type,
            location: Some(right.span().start),
            context: None,
          })?;
        bool_type
      }
    };

    expr.set_type_annotation(result_type.clone());
    Ok(result_type)
  }

  /// Type check a unary operation
  fn type_check_unary_operation(
    &mut self,
    operator: &UnaryOperator,
    operand: &mut Expression,
    expr: &mut Expression,
  ) -> Result<Type, TypeCheckError> {
    let operand_type = self.type_check_expression(operand)?;

    let result_type = match operator {
      UnaryOperator::Negate => {
        // Negation
        let int_type = Type::primitive(PrimitiveType::Int);
        self.inference.unify(&operand_type, &int_type)
          .map_err(|e| TypeCheckError::TypeMismatch {
            expected: int_type.clone(),
            actual: operand_type,
            location: Some(operand.span().start),
            context: None,
          })?;
        int_type
      }
      UnaryOperator::Not => {
        // Logical not
        let bool_type = Type::primitive(PrimitiveType::Bool);
        self.inference.unify(&operand_type, &bool_type)
          .map_err(|e| TypeCheckError::TypeMismatch {
            expected: bool_type.clone(),
            actual: operand_type,
            location: Some(operand.span().start),
            context: None,
          })?;
        bool_type
      }
    };

    expr.set_type_annotation(result_type.clone());
    Ok(result_type)
  }
}

impl Default for TypeAnnotator {
  fn default() -> Self {
    Self::new()
  }
}
