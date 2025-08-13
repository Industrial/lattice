//! Algorithm W implementation for Hindley-Milner type inference.
//!
//! This module provides the core type inference engine that implements
//! Algorithm W to traverse the AST and infer types for all expressions.

use std::collections::HashMap;
use crate::parser::ast::{Expression, Literal, Span, Identifier, Pattern, Binding, MatchArm, HandlerCase};
use crate::parser::ast::{BinaryOperator, UnaryOperator};
use super::types::{Type, PrimitiveType, TypeLocation};
use super::environment::TypeEnvironment;
use super::inference::{TypeInference, Substitution, UnificationError};

/// Represents a type inference result with inferred type and constraints
#[derive(Debug, Clone)]
pub struct InferenceResult {
    /// The inferred type
    pub inferred_type: Type,
    /// Generated type constraints
    pub constraints: Vec<TypeConstraint>,
    /// Applied substitution
    pub substitution: Substitution,
}

/// Represents a type constraint that must be satisfied
#[derive(Debug, Clone)]
pub struct TypeConstraint {
    pub left: Type,
    pub right: Type,
    pub span: Span,
    pub description: String,
}

impl TypeConstraint {
    pub fn new(left: Type, right: Type, span: Span, description: String) -> Self {
        Self {
            left,
            right,
            span,
            description,
        }
    }
}

/// The main Algorithm W type inference engine
pub struct AlgorithmW {
    /// Type inference engine for unification
    inference: TypeInference,
    /// Current type environment
    environment: TypeEnvironment,
    /// Next available type variable ID
    next_var_id: u64,
}

impl AlgorithmW {
    /// Create a new Algorithm W engine
    pub fn new() -> Self {
        Self {
            inference: TypeInference::new(),
            environment: TypeEnvironment::new(),
            next_var_id: 0,
        }
    }

    /// Create a new Algorithm W engine with a specific environment
    pub fn with_environment(environment: TypeEnvironment) -> Self {
        Self {
            inference: TypeInference::with_environment(environment.clone()),
            environment,
            next_var_id: 0,
        }
    }

    /// Generate a fresh type variable
    fn fresh_type_variable(&mut self) -> Type {
        let var_id = self.next_var_id;
        self.next_var_id += 1;
        Type::var(var_id)
    }

    /// Generate a fresh type variable with a name
    fn fresh_named_type_variable(&mut self, name: String) -> Type {
        let var_id = self.next_var_id;
        self.next_var_id += 1;
        Type::named_var(var_id, name)
    }

    /// Infer the type of an expression using Algorithm W
    pub fn infer_expression(&mut self, expr: &Expression) -> Result<InferenceResult, UnificationError> {
        match expr {
            Expression::Literal { literal, span } => {
                self.infer_literal(literal, span)
            }
            Expression::Variable { identifier, span } => {
                self.infer_variable(identifier, span)
            }
            Expression::Application { function, arguments, span } => {
                self.infer_application(function, arguments, span)
            }
            Expression::BinaryOp { left, operator, right, span } => {
                self.infer_binary_op(left, operator, right, span)
            }
            Expression::UnaryOp { operator, operand, span } => {
                self.infer_unary_op(operator, operand, span)
            }
            Expression::Let { bindings, body, span } => {
                self.infer_let(bindings, body, span)
            }
            Expression::If { condition, then_branch, else_branch, span } => {
                self.infer_if(condition, then_branch, else_branch, span)
            }
            Expression::Match { scrutinee, arms, span } => {
                self.infer_match(scrutinee, arms, span)
            }
            Expression::Lambda { parameters, body, span } => {
                self.infer_lambda(parameters, body, span)
            }
            Expression::Tuple { elements, span } => {
                self.infer_tuple(elements, span)
            }
            Expression::List { elements, span } => {
                self.infer_list(elements, span)
            }
            Expression::EffectOp { operation, arguments, span } => {
                self.infer_effect_op(operation, arguments, span)
            }
            Expression::Handler { expression, cases, span } => {
                self.infer_handler(expression, cases, span)
            }
        }
    }

    /// Infer type for literal expressions
    fn infer_literal(&mut self, literal: &Literal, span: &Span) -> Result<InferenceResult, UnificationError> {
        let inferred_type = match literal {
            Literal::Integer { .. } => Type::primitive(PrimitiveType::Int),
            Literal::Float { .. } => Type::primitive(PrimitiveType::Float),
            Literal::String { .. } => Type::primitive(PrimitiveType::String),
            Literal::Char { .. } => Type::primitive(PrimitiveType::Char),
            Literal::Boolean { .. } => Type::primitive(PrimitiveType::Bool),
            Literal::Unit { .. } => Type::primitive(PrimitiveType::Unit),
        };

        Ok(InferenceResult {
            inferred_type,
            constraints: Vec::new(),
            substitution: Substitution::new(),
        })
    }

    /// Infer type for variable expressions
    fn infer_variable(&mut self, identifier: &Identifier, span: &Span) -> Result<InferenceResult, UnificationError> {
        // Look up the variable in the current environment
        if let Some(binding) = self.environment.lookup(&identifier.name) {
            Ok(InferenceResult {
                inferred_type: binding.ty.clone(),
                constraints: Vec::new(),
                substitution: Substitution::new(),
            })
        } else {
            // Variable not found, create a fresh type variable
            let fresh_type = self.fresh_named_type_variable(identifier.name.clone());
            Ok(InferenceResult {
                inferred_type: fresh_type,
                constraints: Vec::new(),
                substitution: Substitution::new(),
            })
        }
    }

    /// Infer type for function application expressions
    fn infer_application(
        &mut self,
        function: &Expression,
        arguments: &[Expression],
        span: &Span,
    ) -> Result<InferenceResult, UnificationError> {
        // Infer the type of the function
        let function_result = self.infer_expression(function)?;
        let function_type = function_result.inferred_type;

        // Infer types for all arguments
        let mut argument_results = Vec::new();
        for arg in arguments {
            let arg_result = self.infer_expression(arg)?;
            argument_results.push(arg_result);
        }

        // Create a fresh type variable for the return type
        let return_type = self.fresh_type_variable();

        // Build the expected function type
        let expected_function_type = if arguments.is_empty() {
            Type::function(vec![], return_type.clone())
        } else {
            let param_types: Vec<Type> = argument_results.iter()
                .map(|r| r.inferred_type.clone())
                .collect();
            Type::function(param_types, return_type.clone())
        };

        // Unify the inferred function type with the expected function type
        let unify_subst = self.inference.unify(&function_type, &expected_function_type)?;

        // Combine all substitutions
        let mut combined_subst = function_result.substitution;
        for arg_result in &argument_results {
            combined_subst = combined_subst.compose(&arg_result.substitution);
        }
        combined_subst = combined_subst.compose(&unify_subst);

        // Apply the substitution to get the final return type
        let final_return_type = combined_subst.apply(&return_type);

        // Collect all constraints
        let mut all_constraints = function_result.constraints;
        for arg_result in &argument_results {
            all_constraints.extend(arg_result.constraints.clone());
        }

        Ok(InferenceResult {
            inferred_type: final_return_type,
            constraints: all_constraints,
            substitution: combined_subst,
        })
    }

    /// Infer type for binary operations
    fn infer_binary_op(
        &mut self,
        left: &Expression,
        operator: &BinaryOperator,
        right: &Expression,
        span: &Span,
    ) -> Result<InferenceResult, UnificationError> {
        // Infer types for left and right operands
        let left_result = self.infer_expression(left)?;
        let right_result = self.infer_expression(right)?;

        // Determine the expected type based on the operator
        let expected_type = match operator {
            BinaryOperator::Add | BinaryOperator::Subtract | BinaryOperator::Multiply | BinaryOperator::Divide => {
                // Arithmetic operators expect numeric types
                Type::primitive(PrimitiveType::Int) // For now, assume Int
            }
            BinaryOperator::Equal | BinaryOperator::NotEqual | BinaryOperator::LessThan | 
            BinaryOperator::LessThanEqual | BinaryOperator::GreaterThan | BinaryOperator::GreaterThanEqual => {
                // Comparison operators return boolean
                Type::primitive(PrimitiveType::Bool)
            }
            BinaryOperator::And | BinaryOperator::Or => {
                // Logical operators expect and return boolean
                Type::primitive(PrimitiveType::Bool)
            }
            _ => {
                // Default to a fresh type variable for unknown operators
                self.fresh_type_variable()
            }
        };

        // Create constraints for operand types
        let mut constraints = Vec::new();
        
        // For arithmetic operators, both operands should be the same numeric type
        if matches!(operator, BinaryOperator::Add | BinaryOperator::Subtract | BinaryOperator::Multiply | BinaryOperator::Divide) {
            constraints.push(TypeConstraint::new(
                left_result.inferred_type.clone(),
                right_result.inferred_type.clone(),
                *span,
                format!("Operands of {} must have the same type", operator),
            ));
        }

        // Combine substitutions
        let mut combined_subst = left_result.substitution;
        combined_subst = combined_subst.compose(&right_result.substitution);

        // Apply substitution to get final types
        let final_left_type = combined_subst.apply(&left_result.inferred_type);
        let final_right_type = combined_subst.apply(&right_result.inferred_type);

        Ok(InferenceResult {
            inferred_type: expected_type,
            constraints,
            substitution: combined_subst,
        })
    }

    /// Infer type for unary operations
    fn infer_unary_op(
        &mut self,
        operator: &UnaryOperator,
        operand: &Expression,
        span: &Span,
    ) -> Result<InferenceResult, UnificationError> {
        // Infer the type of the operand
        let operand_result = self.infer_expression(operand)?;

        // Determine the result type based on the operator
        let result_type = match operator {
            UnaryOperator::Not => Type::primitive(PrimitiveType::Bool),
            UnaryOperator::Negate => operand_result.inferred_type.clone(),
            _ => self.fresh_type_variable(),
        };

        Ok(InferenceResult {
            inferred_type: result_type,
            constraints: operand_result.constraints,
            substitution: operand_result.substitution,
        })
    }

    /// Infer type for let expressions
    fn infer_let(
        &mut self,
        bindings: &[Binding],
        body: &Expression,
        span: &Span,
    ) -> Result<InferenceResult, UnificationError> {
        // Create a new environment for the let expression
        let mut let_env = self.environment.child();

        // Process each binding
        for binding in bindings {
            // Infer the type of the binding value
            let value_result = self.infer_expression(&binding.value)?;
            
            // Add the binding to the environment
            let_env.bind(binding.name.clone(), value_result.inferred_type.clone())?;
        }

        // Create a new Algorithm W instance with the let environment
        let mut let_inference = AlgorithmW::with_environment(let_env);
        let_inference.next_var_id = self.next_var_id;

        // Infer the type of the body
        let body_result = let_inference.infer_expression(body)?;

        // Update our next_var_id
        self.next_var_id = let_inference.next_var_id;

        Ok(InferenceResult {
            inferred_type: body_result.inferred_type,
            constraints: body_result.constraints,
            substitution: body_result.substitution,
        })
    }

    /// Infer type for if expressions
    fn infer_if(
        &mut self,
        condition: &Expression,
        then_branch: &Expression,
        else_branch: &Expression,
        span: &Span,
    ) -> Result<InferenceResult, UnificationError> {
        // Infer the type of the condition (should be boolean)
        let condition_result = self.infer_expression(condition)?;
        
        // Infer types for both branches
        let then_result = self.infer_expression(then_branch)?;
        let else_result = self.infer_expression(else_branch)?;

        // Create constraint that both branches should have the same type
        let mut constraints = Vec::new();
        constraints.push(TypeConstraint::new(
            then_result.inferred_type.clone(),
            else_result.inferred_type.clone(),
            *span,
            "Both branches of if expression must have the same type".to_string(),
        ));

        // Combine substitutions
        let mut combined_subst = condition_result.substitution;
        combined_subst = combined_subst.compose(&then_result.substitution);
        combined_subst = combined_subst.compose(&else_result.substitution);

        // The result type is the type of the then branch (after unification)
        let result_type = combined_subst.apply(&then_result.inferred_type);

        Ok(InferenceResult {
            inferred_type: result_type,
            constraints,
            substitution: combined_subst,
        })
    }

    /// Infer type for lambda expressions
    fn infer_lambda(
        &mut self,
        parameters: &[Pattern],
        body: &Expression,
        span: &Span,
    ) -> Result<InferenceResult, UnificationError> {
        // Create a new environment for the lambda
        let mut lambda_env = self.environment.child();

        // Process parameters (for now, assume they're simple identifiers)
        let mut param_types = Vec::new();
        for param in parameters {
            // Create a fresh type variable for each parameter
            let param_type = self.fresh_type_variable();
            param_types.push(param_type.clone());

            // Add to environment (assuming simple identifier patterns for now)
            if let Pattern::Variable { identifier, .. } = param {
                lambda_env.bind(identifier.name.clone(), param_type)?;
            }
        }

        // Create a new Algorithm W instance with the lambda environment
        let mut lambda_inference = AlgorithmW::with_environment(lambda_env);
        lambda_inference.next_var_id = self.next_var_id;

        // Infer the type of the body
        let body_result = lambda_inference.infer_expression(body)?;

        // Update our next_var_id
        self.next_var_id = lambda_inference.next_var_id;

        // Create the function type
        let function_type = Type::function(param_types, body_result.inferred_type);

        Ok(InferenceResult {
            inferred_type: function_type,
            constraints: body_result.constraints,
            substitution: body_result.substitution,
        })
    }

    /// Infer type for tuple expressions
    fn infer_tuple(
        &mut self,
        elements: &[Expression],
        span: &Span,
    ) -> Result<InferenceResult, UnificationError> {
        // Infer types for all elements
        let mut element_results = Vec::new();
        for element in elements {
            let element_result = self.infer_expression(element)?;
            element_results.push(element_result);
        }

        // Combine all substitutions
        let mut combined_subst = Substitution::new();
        for element_result in &element_results {
            combined_subst = combined_subst.compose(&element_result.substitution);
        }

        // Build the tuple type
        let element_types: Vec<Type> = element_results.iter()
            .map(|r| combined_subst.apply(&r.inferred_type))
            .collect();
        let tuple_type = Type::tuple(element_types);

        Ok(InferenceResult {
            inferred_type: tuple_type,
            constraints: Vec::new(), // No additional constraints for tuples
            substitution: combined_subst,
        })
    }

    /// Infer type for list expressions
    fn infer_list(
        &mut self,
        elements: &[Expression],
        span: &Span,
    ) -> Result<InferenceResult, UnificationError> {
        if elements.is_empty() {
            // Empty list gets a polymorphic type
            let element_type = self.fresh_type_variable();
            let list_type = Type::application("List".to_string(), vec![element_type]);
            
            Ok(InferenceResult {
                inferred_type: list_type,
                constraints: Vec::new(),
                substitution: Substitution::new(),
            })
        } else {
            // Infer types for all elements
            let mut element_results = Vec::new();
            for element in elements {
                let element_result = self.infer_expression(element)?;
                element_results.push(element_result);
            }

            // All elements should have the same type
            let first_type = element_results[0].inferred_type.clone();
            let mut constraints = Vec::new();
            
            for (i, element_result) in element_results.iter().enumerate().skip(1) {
                constraints.push(TypeConstraint::new(
                    first_type.clone(),
                    element_result.inferred_type.clone(),
                    *span,
                    format!("List element {} must have the same type as first element", i),
                ));
            }

            // Combine all substitutions
            let mut combined_subst = Substitution::new();
            for element_result in &element_results {
                combined_subst = combined_subst.compose(&element_result.substitution);
            }

            // Create the list type
            let final_element_type = combined_subst.apply(&first_type);
            let list_type = Type::application("List".to_string(), vec![final_element_type]);

            Ok(InferenceResult {
                inferred_type: list_type,
                constraints,
                substitution: combined_subst,
            })
        }
    }

    /// Infer type for effect operations
    fn infer_effect_op(
        &mut self,
        operation: &Identifier,
        arguments: &[Expression],
        span: &Span,
    ) -> Result<InferenceResult, UnificationError> {
        // For now, assume effect operations return a fresh type variable
        // In a full implementation, this would look up the effect signature
        let result_type = self.fresh_type_variable();

        // Infer types for arguments
        let mut arg_results = Vec::new();
        for arg in arguments {
            let arg_result = self.infer_expression(arg)?;
            arg_results.push(arg_result);
        }

        // Combine substitutions
        let mut combined_subst = Substitution::new();
        for arg_result in &arg_results {
            combined_subst = combined_subst.compose(&arg_result.substitution);
        }

        Ok(InferenceResult {
            inferred_type: result_type,
            constraints: Vec::new(),
            substitution: combined_subst,
        })
    }

    /// Infer type for match expressions
    fn infer_match(
        &mut self,
        scrutinee: &Expression,
        arms: &[MatchArm],
        span: &Span,
    ) -> Result<InferenceResult, UnificationError> {
        // Infer the type of the scrutinee
        let scrutinee_result = self.infer_expression(scrutinee)?;

        // Infer types for all match arms
        let mut arm_results = Vec::new();
        for arm in arms {
            let arm_result = self.infer_expression(&arm.body)?;
            arm_results.push(arm_result);
        }

        // All arms should have the same type
        if arm_results.is_empty() {
            return Err(UnificationError::TypeMismatch {
                expected: Type::primitive(PrimitiveType::Unit),
                actual: Type::primitive(PrimitiveType::Unit),
                location: None,
            });
        }

        let first_arm_type = arm_results[0].inferred_type.clone();
        let mut constraints = Vec::new();

        // Create constraints that all arms have the same type
        for (i, arm_result) in arm_results.iter().enumerate().skip(1) {
            constraints.push(TypeConstraint::new(
                first_arm_type.clone(),
                arm_result.inferred_type.clone(),
                *span,
                format!("Match arm {} must have the same type as first arm", i),
            ));
        }

        // Combine all substitutions
        let mut combined_subst = scrutinee_result.substitution;
        for arm_result in &arm_results {
            combined_subst = combined_subst.compose(&arm_result.substitution);
        }

        // The result type is the type of the first arm (after unification)
        let result_type = combined_subst.apply(&first_arm_type);

        // Collect all constraints
        let mut all_constraints = scrutinee_result.constraints;
        all_constraints.extend(constraints);

        Ok(InferenceResult {
            inferred_type: result_type,
            constraints: all_constraints,
            substitution: combined_subst,
        })
    }

    /// Infer type for effect handlers
    fn infer_handler(
        &mut self,
        expression: &Expression,
        cases: &[HandlerCase],
        span: &Span,
    ) -> Result<InferenceResult, UnificationError> {
        // Infer the type of the expression being handled
        let expr_result = self.infer_expression(expression)?;

        // For now, assume the handler returns the same type as the expression
        // In a full implementation, this would be more complex
        let result_type = expr_result.inferred_type.clone();

        Ok(InferenceResult {
            inferred_type: result_type,
            constraints: expr_result.constraints,
            substitution: expr_result.substitution,
        })
    }

    /// Get the current environment
    pub fn environment(&self) -> &TypeEnvironment {
        &self.environment
    }

    /// Get a mutable reference to the current environment
    pub fn environment_mut(&mut self) -> &mut TypeEnvironment {
        &mut self.environment
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::ast::{Expression, Literal, Span, Identifier, SourceLocation};

    fn create_test_span() -> Span {
        let loc = SourceLocation::new(1, 1);
        Span::new(loc, loc)
    }

    #[test]
    fn test_infer_literal_integer() {
        let mut engine = AlgorithmW::new();
        let literal = Expression::Literal {
            literal: Literal::Integer { value: 42, span: create_test_span() },
            span: create_test_span(),
        };

        let result = engine.infer_expression(&literal).unwrap();
        assert!(matches!(result.inferred_type, Type::Primitive(PrimitiveType::Int)));
    }

    #[test]
    fn test_infer_literal_boolean() {
        let mut engine = AlgorithmW::new();
        let literal = Expression::Literal {
            literal: Literal::Boolean { value: true, span: create_test_span() },
            span: create_test_span(),
        };

        let result = engine.infer_expression(&literal).unwrap();
        assert!(matches!(result.inferred_type, Type::Primitive(PrimitiveType::Bool)));
    }

    #[test]
    fn test_infer_variable() {
        let mut engine = AlgorithmW::new();
        let identifier = Identifier { name: "x".to_string(), span: create_test_span() };
        let variable = Expression::Variable { identifier, span: create_test_span() };

        let result = engine.infer_expression(&variable).unwrap();
        assert!(result.inferred_type.contains_variables());
    }

    #[test]
    fn test_infer_tuple() {
        let mut engine = AlgorithmW::new();
        let elements = vec![
            Expression::Literal {
                literal: Literal::Integer { value: 1, span: create_test_span() },
                span: create_test_span(),
            },
            Expression::Literal {
                literal: Literal::Boolean { value: true, span: create_test_span() },
                span: create_test_span(),
            },
        ];
        let tuple = Expression::Tuple { elements, span: create_test_span() };

        let result = engine.infer_expression(&tuple).unwrap();
        assert!(matches!(result.inferred_type, Type::Product(_)));
    }
} 