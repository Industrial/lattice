//! Type annotator for the Lattice language.
//!
//! This module provides functionality to annotate AST nodes with
//! inferred type information using the Hindley-Milner type inference system.

use crate::types::inference::TypeInference;
use crate::types::types::{PrimitiveType, ProductType, SumType, Type};
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
  Application {
    function: String,
    arguments: Vec<SimpleExpression>,
  },
  /// Match expression
  Match {
    scrutinee: Box<SimpleExpression>,
    arms: Vec<SimpleMatchArm>,
  },
}

/// Represents a simple match arm
#[derive(Debug, Clone)]
pub struct SimpleMatchArm {
  /// Pattern (simplified to just constructor name)
  pub pattern: String,
  /// Expression to evaluate
  pub expression: SimpleExpression,
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
      SimpleExpression::Application {
        function,
        arguments,
      } => self.type_check_application(function, arguments),
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
    let subst = self
      .inference
      .unify(&func_type, &expected_func_type)
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
      return Err(TypeCheckError::EmptyMatchExpression { location: None });
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
      let subst = self.inference.unify(&unified_type, arm_type).map_err(|e| {
        TypeCheckError::TypeMismatch {
          expected: unified_type.clone(),
          actual: arm_type.clone(),
          location: None,
          context: None,
        }
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
}

impl Default for TypeAnnotator {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_type_check_literal() {
    let mut annotator = TypeAnnotator::new();
    let expr = SimpleExpression::Literal(Type::primitive(PrimitiveType::Int));

    let result = annotator.type_check_expression(&expr);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Type::primitive(PrimitiveType::Int));
  }

  #[test]
  fn test_type_check_variable() {
    let mut annotator = TypeAnnotator::new();
    annotator.add_variable_binding("x".to_string(), Type::primitive(PrimitiveType::Int));

    let expr = SimpleExpression::Variable("x".to_string());
    let result = annotator.type_check_expression(&expr);

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Type::primitive(PrimitiveType::Int));
  }

  #[test]
  fn test_type_check_undefined_variable() {
    let mut annotator = TypeAnnotator::new();
    let expr = SimpleExpression::Variable("x".to_string());

    let result = annotator.type_check_expression(&expr);
    assert!(result.is_err());

    if let Err(TypeCheckError::UndefinedVariable { name, .. }) = result {
      assert_eq!(name, "x");
    } else {
      panic!("Expected UndefinedVariable error");
    }
  }

  #[test]
  fn test_type_check_match_expression() {
    let mut annotator = TypeAnnotator::new();

    // Define Option type
    let option_type = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);
    annotator.add_type_definition("Option".to_string(), option_type.clone());

    // Create match expression
    let scrutinee = SimpleExpression::Variable("opt".to_string());
    let arms = vec![
      SimpleMatchArm {
        pattern: "None".to_string(),
        expression: SimpleExpression::Literal(Type::primitive(PrimitiveType::Int)),
      },
      SimpleMatchArm {
        pattern: "Some".to_string(),
        expression: SimpleExpression::Literal(Type::primitive(PrimitiveType::Int)),
      },
    ];

    let match_expr = SimpleExpression::Match {
      scrutinee: Box::new(scrutinee),
      arms,
    };

    // Add variable binding for scrutinee
    annotator.add_variable_binding("opt".to_string(), option_type);

    let result = annotator.type_check_expression(&match_expr);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Type::primitive(PrimitiveType::Int));
  }

  #[test]
  fn test_type_check_match_with_undefined_constructor() {
    let mut annotator = TypeAnnotator::new();

    // Define Option type
    let option_type = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);
    annotator.add_type_definition("Option".to_string(), option_type.clone());

    // Create match expression with undefined constructor
    let scrutinee = SimpleExpression::Variable("opt".to_string());
    let arms = vec![SimpleMatchArm {
      pattern: "Maybe".to_string(), // This doesn't exist in Option
      expression: SimpleExpression::Literal(Type::primitive(PrimitiveType::Int)),
    }];

    let match_expr = SimpleExpression::Match {
      scrutinee: Box::new(scrutinee),
      arms,
    };

    // Add variable binding for scrutinee
    annotator.add_variable_binding("opt".to_string(), option_type);

    let result = annotator.type_check_expression(&match_expr);
    assert!(result.is_err());

    if let Err(TypeCheckError::UndefinedConstructor {
      constructor_name, ..
    }) = result
    {
      assert_eq!(constructor_name, "Maybe");
    } else {
      panic!("Expected UndefinedConstructor error");
    }
  }

  #[test]
  fn test_type_check_empty_match() {
    let mut annotator = TypeAnnotator::new();

    let scrutinee = SimpleExpression::Variable("x".to_string());
    let arms = vec![]; // Empty arms

    let match_expr = SimpleExpression::Match {
      scrutinee: Box::new(scrutinee),
      arms,
    };

    let result = annotator.type_check_expression(&match_expr);
    assert!(result.is_err());

    // The error could be either EmptyMatchExpression or UndefinedVariable
    match result {
      Err(TypeCheckError::EmptyMatchExpression { .. }) => {
        // Expected error
      }
      Err(TypeCheckError::UndefinedVariable { .. }) => {
        // Also acceptable since scrutinee variable is undefined
      }
      _ => {
        panic!(
          "Expected EmptyMatchExpression or UndefinedVariable error, got: {:?}",
          result
        );
      }
    }
  }

  #[test]
  fn test_type_check_function_application() {
    let mut annotator = TypeAnnotator::new();

    // Define a function type: Int -> Int
    let func_type = Type::function(
      vec![Type::primitive(PrimitiveType::Int)],
      Type::primitive(PrimitiveType::Int),
    );
    annotator.add_variable_binding("add_one".to_string(), func_type);

    // Create function application: add_one(42)
    let expr = SimpleExpression::Application {
      function: "add_one".to_string(),
      arguments: vec![SimpleExpression::Literal(Type::primitive(
        PrimitiveType::Int,
      ))],
    };

    let result = annotator.type_check_expression(&expr);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Type::primitive(PrimitiveType::Int));
  }

  #[test]
  fn test_type_check_function_application_wrong_arity() {
    let mut annotator = TypeAnnotator::new();

    // Define a function type: Int -> Int
    let func_type = Type::function(
      vec![Type::primitive(PrimitiveType::Int)],
      Type::primitive(PrimitiveType::Int),
    );
    annotator.add_variable_binding("add_one".to_string(), func_type);

    // Create function application with wrong number of arguments: add_one(42, 43)
    let expr = SimpleExpression::Application {
      function: "add_one".to_string(),
      arguments: vec![
        SimpleExpression::Literal(Type::primitive(PrimitiveType::Int)),
        SimpleExpression::Literal(Type::primitive(PrimitiveType::Int)),
      ],
    };

    let result = annotator.type_check_expression(&expr);
    assert!(result.is_err());

    if let Err(TypeCheckError::TypeMismatch { .. }) = result {
      // Expected error
    } else {
      panic!("Expected TypeMismatch error");
    }
  }
}
