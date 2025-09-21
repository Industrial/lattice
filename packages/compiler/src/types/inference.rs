//! Type inference for the Lattice language.
//!
//! This module provides the core type inference engine that implements
//! Algorithm W for Hindley-Milner type inference, including unification,
//! constraint generation, and type generalization.

use super::environment::TypeEnvironment;
use super::types::{Type, TypeLocation};
use std::collections::HashMap;
use std::fmt;

/// Represents a type constraint that must be satisfied
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeConstraint {
  pub left: Type,
  pub right: Type,
  pub location: Option<TypeLocation>,
  pub description: Option<String>,
}

impl TypeConstraint {
  pub fn new(left: Type, right: Type) -> Self {
    Self {
      left,
      right,
      location: None,
      description: None,
    }
  }

  pub fn with_location(mut self, location: TypeLocation) -> Self {
    self.location = Some(location);
    self
  }

  pub fn with_description(mut self, description: String) -> Self {
    self.description = Some(description);
    self
  }
}

impl fmt::Display for TypeConstraint {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} = {}", self.left, self.right)?;
    if let Some(ref desc) = self.description {
      write!(f, " ({})", desc)?;
    }
    Ok(())
  }
}

/// Represents a substitution that maps type variables to types
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Substitution {
  mappings: HashMap<u64, Type>,
}

impl Substitution {
  pub fn new() -> Self {
    Self {
      mappings: HashMap::new(),
    }
  }

  /// Add a mapping from a type variable to a type
  pub fn bind(&mut self, var_id: u64, ty: Type) -> Result<(), String> {
    // Check for occurs check to prevent infinite types
    if ty.contains_variables() {
      let vars = ty.get_variables();
      if vars.iter().any(|v| v.id == var_id) {
        return Err(format!(
          "Occurs check failed: type variable {} would create infinite type",
          var_id
        ));
      }
    }

    self.mappings.insert(var_id, ty);
    Ok(())
  }

  /// Apply this substitution to a type
  pub fn apply(&self, ty: &Type) -> Type {
    match ty {
      Type::Variable(var) => {
        if let Some(substituted) = self.mappings.get(&var.id) {
          substituted.clone()
        } else {
          ty.clone()
        }
      }
      Type::Function(func) => {
        let new_params: Vec<Type> = func
          .parameters
          .iter()
          .map(|param| self.apply(param))
          .collect();
        let new_return = self.apply(&func.return_type);
        Type::Function(super::types::FunctionType::new(new_params, new_return))
      }
      Type::Product(product) => {
        let new_fields: Vec<(Option<String>, Type)> = product
          .fields
          .iter()
          .map(|(name, field_type)| (name.clone(), self.apply(field_type)))
          .collect();
        Type::Product(super::types::ProductType::new(new_fields))
      }
      Type::Sum(sum) => {
        let new_variants: Vec<(String, Option<Type>)> = sum
          .variants
          .iter()
          .map(|(name, variant_type)| (name.clone(), variant_type.as_ref().map(|t| self.apply(t))))
          .collect();
        Type::Sum(super::types::SumType::new(new_variants))
      }
      Type::Effect(effect) => {
        let new_params: Vec<Type> = effect
          .parameters
          .iter()
          .map(|param| self.apply(param))
          .collect();
        Type::Effect(super::types::EffectType::new(
          effect.effect_name.clone(),
          new_params,
        ))
      }
      Type::Application(app) => {
        let new_args: Vec<Type> = app.arguments.iter().map(|arg| self.apply(arg)).collect();
        Type::Application(super::types::TypeApplication::new(
          app.constructor.clone(),
          new_args,
        ))
      }
      Type::Reference(reference) => {
        let new_inner = self.apply(&reference.inner_type);
        if reference.mutable {
          Type::mutable_reference(new_inner)
        } else {
          Type::reference(new_inner)
        }
      }
      Type::Constrained(constrained) => {
        let new_base = self.apply(&constrained.base_type);
        Type::constrained(new_base, constrained.constraint.clone())
      }
      Type::Alias(name, base) => {
        let new_base = self.apply(base);
        Type::alias(name.clone(), new_base)
      }
      Type::Generic(name, params) => {
        let new_params: Vec<Type> = params.iter().map(|param| self.apply(param)).collect();
        Type::generic(name.clone(), new_params)
      }
      Type::Polymorphic(poly) => {
        let new_body = self.apply(&poly.body);
        Type::polymorphic(poly.variables.clone(), new_body)
      }
      // Primitive types don't need substitution
      _ => ty.clone(),
    }
  }

  /// Compose this substitution with another
  pub fn compose(&self, other: &Substitution) -> Substitution {
    let mut composed = self.clone();

    for (var_id, ty) in &other.mappings {
      let substituted = composed.apply(ty);
      composed.mappings.insert(*var_id, substituted);
    }

    composed
  }

  /// Get the number of mappings in this substitution
  pub fn len(&self) -> usize {
    self.mappings.len()
  }

  /// Check if this substitution is empty
  pub fn is_empty(&self) -> bool {
    self.mappings.is_empty()
  }

  /// Get all type variables that are mapped
  pub fn mapped_variables(&self) -> Vec<u64> {
    self.mappings.keys().cloned().collect()
  }
}

impl fmt::Display for Substitution {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.mappings.is_empty() {
      write!(f, "{}", "{}")
    } else {
      let mappings: Vec<String> = self
        .mappings
        .iter()
        .map(|(var_id, ty)| format!("α{} → {}", var_id, ty))
        .collect();
      write!(f, "{{{}}}", mappings.join(", "))
    }
  }
}

/// Represents a unification error
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnificationError {
  /// Type mismatch between two types
  TypeMismatch {
    expected: Type,
    actual: Type,
    location: Option<TypeLocation>,
  },
  /// Occurs check failure (infinite type)
  OccursCheck {
    var_id: u64,
    ty: Type,
    location: Option<TypeLocation>,
  },
  /// Incompatible function arities
  ArityMismatch {
    expected: usize,
    actual: usize,
    location: Option<TypeLocation>,
  },
  /// Incompatible record fields
  FieldMismatch {
    field: String,
    expected: Type,
    actual: Type,
    location: Option<TypeLocation>,
  },
  /// Incompatible sum variants
  VariantMismatch {
    variant: String,
    expected: Option<Type>,
    actual: Option<Type>,
    location: Option<TypeLocation>,
  },
  /// Binding error (e.g., duplicate variable names)
  BindingError {
    message: String,
    location: Option<TypeLocation>,
  },
}

impl fmt::Display for UnificationError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      UnificationError::TypeMismatch {
        expected,
        actual,
        location,
      } => {
        write!(f, "Type mismatch: expected {}, got {}", expected, actual)?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
      UnificationError::OccursCheck {
        var_id,
        ty,
        location,
      } => {
        write!(
          f,
          "Occurs check failed: type variable α{} would create infinite type {}",
          var_id, ty
        )?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
      UnificationError::ArityMismatch {
        expected,
        actual,
        location,
      } => {
        write!(
          f,
          "Arity mismatch: expected {} parameters, got {}",
          expected, actual
        )?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
      UnificationError::FieldMismatch {
        field,
        expected,
        actual,
        location,
      } => {
        write!(
          f,
          "Field '{}' type mismatch: expected {}, got {}",
          field, expected, actual
        )?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
      UnificationError::VariantMismatch {
        variant,
        expected,
        actual,
        location,
      } => {
        write!(
          f,
          "Variant '{}' type mismatch: expected {:?}, got {:?}",
          variant, expected, actual
        )?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
      UnificationError::BindingError { message, location } => {
        write!(f, "Binding error: {}", message)?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
    }
  }
}

impl From<String> for UnificationError {
  fn from(message: String) -> Self {
    UnificationError::BindingError {
      message,
      location: None,
    }
  }
}

/// The main type inference engine
#[derive(Debug)]
pub struct TypeInference {
  /// Next available type variable ID
  next_var_id: u64,
  /// Current substitution
  substitution: Substitution,
  /// Type environment
  environment: TypeEnvironment,
}

impl TypeInference {
  /// Create a new type inference engine
  pub fn new() -> Self {
    Self {
      next_var_id: 0,
      substitution: Substitution::new(),
      environment: TypeEnvironment::new(),
    }
  }

  /// Create a new type inference engine with a specific environment
  pub fn with_environment(environment: TypeEnvironment) -> Self {
    Self {
      next_var_id: 0,
      substitution: Substitution::new(),
      environment,
    }
  }

  /// Generate a fresh type variable
  pub fn fresh_type_variable(&mut self) -> Type {
    let var_id = self.next_var_id;
    self.next_var_id += 1;
    Type::var(var_id)
  }

  /// Generate a fresh type variable with a name
  pub fn fresh_named_type_variable(&mut self, name: String) -> Type {
    let var_id = self.next_var_id;
    self.next_var_id += 1;
    Type::named_var(var_id, name)
  }

  /// Unify two types, returning a substitution if successful
  pub fn unify(&mut self, left: &Type, right: &Type) -> Result<Substitution, UnificationError> {
    match (left, right) {
      // Same types unify trivially
      (l, r) if l == r => Ok(Substitution::new()),

      // Type variable on left
      (Type::Variable(var), ty) => self.unify_variable(var.id, ty.clone()),

      // Type variable on right
      (ty, Type::Variable(var)) => self.unify_variable(var.id, ty.clone()),

      // Function types
      (Type::Function(left_func), Type::Function(right_func)) => {
        self.unify_function_types(left_func, right_func)
      }

      // Product types
      (Type::Product(left_product), Type::Product(right_product)) => {
        self.unify_product_types(left_product, right_product)
      }

      // Sum types
      (Type::Sum(left_sum), Type::Sum(right_sum)) => self.unify_sum_types(left_sum, right_sum),

      // Effect types
      (Type::Effect(left_effect), Type::Effect(right_effect)) => {
        self.unify_effect_types(left_effect, right_effect)
      }

      // Type applications
      (Type::Application(left_app), Type::Application(right_app)) => {
        self.unify_type_applications(left_app, right_app)
      }

      // Reference types
      (Type::Reference(left_ref), Type::Reference(right_ref)) => {
        if left_ref.mutable == right_ref.mutable {
          self.unify(&left_ref.inner_type, &right_ref.inner_type)
        } else {
          Err(UnificationError::TypeMismatch {
            expected: left.clone(),
            actual: right.clone(),
            location: None,
          })
        }
      }

      // Constrained types
      (Type::Constrained(left_constrained), Type::Constrained(right_constrained)) => {
        self.unify(&left_constrained.base_type, &right_constrained.base_type)
      }

      // Type aliases
      (Type::Alias(_, left_base), Type::Alias(_, right_base)) => self.unify(left_base, right_base),

      // Generic types
      (Type::Generic(left_name, left_params), Type::Generic(right_name, right_params)) => {
        if left_name == right_name && left_params.len() == right_params.len() {
          self.unify_sequences(left_params, right_params)
        } else {
          Err(UnificationError::TypeMismatch {
            expected: left.clone(),
            actual: right.clone(),
            location: None,
          })
        }
      }

      // Polymorphic types
      (Type::Polymorphic(left_poly), Type::Polymorphic(right_poly)) => {
        if left_poly.variables.len() == right_poly.variables.len() {
          self.unify(&left_poly.body, &right_poly.body)
        } else {
          Err(UnificationError::TypeMismatch {
            expected: left.clone(),
            actual: right.clone(),
            location: None,
          })
        }
      }

      // Incompatible types
      _ => Err(UnificationError::TypeMismatch {
        expected: left.clone(),
        actual: right.clone(),
        location: None,
      }),
    }
  }

  /// Unify a type variable with a type
  fn unify_variable(&mut self, var_id: u64, ty: Type) -> Result<Substitution, UnificationError> {
    // Check if the variable is already bound
    if let Some(bound_type) = self.substitution.mappings.get(&var_id) {
      let bound_type_clone = bound_type.clone();
      return self.unify(&bound_type_clone, &ty);
    }

    // Check for occurs check
    if ty.contains_variables() {
      let vars = ty.get_variables();
      if vars.iter().any(|v| v.id == var_id) {
        return Err(UnificationError::OccursCheck {
          var_id,
          ty,
          location: None,
        });
      }
    }

    // Create a new substitution
    let mut new_subst = Substitution::new();
    new_subst
      .bind(var_id, ty.clone())
      .map_err(|_| UnificationError::OccursCheck {
        var_id,
        ty,
        location: None,
      })?;
    Ok(new_subst)
  }

  /// Unify function types
  fn unify_function_types(
    &mut self,
    left: &super::types::FunctionType,
    right: &super::types::FunctionType,
  ) -> Result<Substitution, UnificationError> {
    if left.parameters.len() != right.parameters.len() {
      return Err(UnificationError::ArityMismatch {
        expected: left.parameters.len(),
        actual: right.parameters.len(),
        location: None,
      });
    }

    // Unify parameter types
    let mut subst = self.unify_sequences(&left.parameters, &right.parameters)?;

    // Unify return types
    let return_subst = self.unify(&left.return_type, &right.return_type)?;
    subst = subst.compose(&return_subst);

    Ok(subst)
  }

  /// Unify product types
  fn unify_product_types(
    &mut self,
    left: &super::types::ProductType,
    right: &super::types::ProductType,
  ) -> Result<Substitution, UnificationError> {
    if left.fields.len() != right.fields.len() {
      return Err(UnificationError::ArityMismatch {
        expected: left.fields.len(),
        actual: right.fields.len(),
        location: None,
      });
    }

    let mut subst = Substitution::new();

    for ((left_name, left_type), (right_name, right_type)) in
      left.fields.iter().zip(right.fields.iter())
    {
      // Check field names if both are named
      if let (Some(ref l_name), Some(ref r_name)) = (left_name, right_name) {
        if l_name != r_name {
          return Err(UnificationError::FieldMismatch {
            field: l_name.clone(),
            expected: left_type.clone(),
            actual: right_type.clone(),
            location: None,
          });
        }
      }

      // Unify field types
      let field_subst = self.unify(left_type, right_type)?;
      subst = subst.compose(&field_subst);
    }

    Ok(subst)
  }

  /// Unify sum types
  fn unify_sum_types(
    &mut self,
    left: &super::types::SumType,
    right: &super::types::SumType,
  ) -> Result<Substitution, UnificationError> {
    if left.variants.len() != right.variants.len() {
      return Err(UnificationError::ArityMismatch {
        expected: left.variants.len(),
        actual: right.variants.len(),
        location: None,
      });
    }

    let mut subst = Substitution::new();

    for ((left_name, left_type), (right_name, right_type)) in
      left.variants.iter().zip(right.variants.iter())
    {
      if left_name != right_name {
        return Err(UnificationError::VariantMismatch {
          variant: left_name.clone(),
          expected: left_type.clone(),
          actual: right_type.clone(),
          location: None,
        });
      }

      // Unify variant types if both have types
      if let (Some(ref l_type), Some(ref r_type)) = (left_type, right_type) {
        let variant_subst = self.unify(l_type, r_type)?;
        subst = subst.compose(&variant_subst);
      }
    }

    Ok(subst)
  }

  /// Unify effect types
  fn unify_effect_types(
    &mut self,
    left: &super::types::EffectType,
    right: &super::types::EffectType,
  ) -> Result<Substitution, UnificationError> {
    if left.effect_name != right.effect_name {
      return Err(UnificationError::TypeMismatch {
        expected: Type::Effect(left.clone()),
        actual: Type::Effect(right.clone()),
        location: None,
      });
    }

    if left.parameters.len() != right.parameters.len() {
      return Err(UnificationError::ArityMismatch {
        expected: left.parameters.len(),
        actual: right.parameters.len(),
        location: None,
      });
    }

    self.unify_sequences(&left.parameters, &right.parameters)
  }

  /// Unify type applications
  fn unify_type_applications(
    &mut self,
    left: &super::types::TypeApplication,
    right: &super::types::TypeApplication,
  ) -> Result<Substitution, UnificationError> {
    if left.constructor != right.constructor {
      return Err(UnificationError::TypeMismatch {
        expected: Type::Application(left.clone()),
        actual: Type::Application(right.clone()),
        location: None,
      });
    }

    if left.arguments.len() != right.arguments.len() {
      return Err(UnificationError::ArityMismatch {
        expected: left.arguments.len(),
        actual: right.arguments.len(),
        location: None,
      });
    }

    self.unify_sequences(&left.arguments, &right.arguments)
  }

  /// Unify sequences of types
  fn unify_sequences(
    &mut self,
    left: &[Type],
    right: &[Type],
  ) -> Result<Substitution, UnificationError> {
    if left.len() != right.len() {
      return Err(UnificationError::ArityMismatch {
        expected: left.len(),
        actual: right.len(),
        location: None,
      });
    }

    let mut subst = Substitution::new();

    for (l, r) in left.iter().zip(right.iter()) {
      let item_subst = self.unify(l, r)?;
      subst = subst.compose(&item_subst);
    }

    Ok(subst)
  }

  /// Get the current substitution
  pub fn substitution(&self) -> &Substitution {
    &self.substitution
  }

  /// Get the current environment
  pub fn environment(&self) -> &TypeEnvironment {
    &self.environment
  }

  /// Get a mutable reference to the current environment
  pub fn environment_mut(&mut self) -> &mut TypeEnvironment {
    &mut self.environment
  }

  /// Apply the current substitution to a type
  pub fn apply_substitution(&self, ty: &Type) -> Type {
    self.substitution.apply(ty)
  }

  /// Get the next available type variable ID
  pub fn next_var_id(&self) -> u64 {
    self.next_var_id
  }

  /// Generalize a type with respect to the current environment
  /// Converts free type variables into universally quantified variables
  pub fn generalize(&self, ty: &Type) -> Type {
    let free_vars = self.get_free_variables(ty);
    if free_vars.is_empty() {
      ty.clone()
    } else {
      Type::polymorphic(free_vars, ty.clone())
    }
  }

  /// Instantiate a polymorphic type by replacing quantified variables with fresh type variables
  pub fn instantiate(&mut self, poly_type: &Type) -> Type {
    match poly_type {
      Type::Polymorphic(poly) => {
        let mut substitution = Substitution::new();

        // Create fresh type variables for each quantified variable
        for var in &poly.variables {
          let fresh_var = self.fresh_type_variable();
          substitution.bind(var.id, fresh_var).unwrap_or_else(|_| {
            // This shouldn't happen with fresh variables
          });
        }

        // Apply the substitution to the body
        substitution.apply(&poly.body)
      }
      _ => poly_type.clone(),
    }
  }

  /// Get free type variables in a type that are not bound in the current environment
  fn get_free_variables(&self, ty: &Type) -> Vec<super::types::TypeVariable> {
    let all_vars = ty.get_variables();
    let mut free_vars = Vec::new();

    for var in all_vars {
      // For now, we consider all type variables as free if they're not in our substitution
      // This is a simplified approach - in a full implementation, we'd track bound variables
      // in the environment more carefully
      if !self.substitution.mappings.contains_key(&var.id) {
        free_vars.push(var);
      }
    }

    free_vars
  }
}

impl Default for TypeInference {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::super::types::{PrimitiveType, Type, TypeVariable};
  use super::*;

  #[test]
  fn test_type_inference_new() {
    let inference = TypeInference::new();
    assert_eq!(inference.next_var_id(), 0);
    assert!(inference.substitution().is_empty());
  }

  #[test]
  fn test_fresh_type_variable() {
    let mut inference = TypeInference::new();
    let var1 = inference.fresh_type_variable();
    let var2 = inference.fresh_type_variable();

    assert_ne!(var1, var2);
    assert_eq!(inference.next_var_id(), 2);
  }

  #[test]
  fn test_unify_same_types() {
    let mut inference = TypeInference::new();
    let int_type = Type::primitive(PrimitiveType::Int);

    let result = inference.unify(&int_type, &int_type);
    assert!(result.is_ok());
    assert!(result.unwrap().is_empty());
  }

  #[test]
  fn test_unify_type_variable_with_type() {
    let mut inference = TypeInference::new();
    let var = inference.fresh_type_variable();
    let int_type = Type::primitive(PrimitiveType::Int);

    let result = inference.unify(&var, &int_type);
    assert!(result.is_ok());

    let subst = result.unwrap();
    assert_eq!(subst.len(), 1);
  }

  #[test]
  fn test_unify_function_types() {
    let mut inference = TypeInference::new();
    let func1 = Type::function(
      vec![Type::primitive(PrimitiveType::Int)],
      Type::primitive(PrimitiveType::Bool),
    );
    let func2 = Type::function(
      vec![Type::primitive(PrimitiveType::Int)],
      Type::primitive(PrimitiveType::Bool),
    );

    let result = inference.unify(&func1, &func2);
    assert!(result.is_ok());
  }

  #[test]
  fn test_unify_function_types_arity_mismatch() {
    let mut inference = TypeInference::new();
    let func1 = Type::function(
      vec![Type::primitive(PrimitiveType::Int)],
      Type::primitive(PrimitiveType::Bool),
    );
    let func2 = Type::function(
      vec![
        Type::primitive(PrimitiveType::Int),
        Type::primitive(PrimitiveType::String),
      ],
      Type::primitive(PrimitiveType::Bool),
    );

    let result = inference.unify(&func1, &func2);
    assert!(result.is_err());

    if let Err(UnificationError::ArityMismatch {
      expected, actual, ..
    }) = result
    {
      assert_eq!(expected, 1);
      assert_eq!(actual, 2);
    } else {
      panic!("Expected ArityMismatch error");
    }
  }

  #[test]
  fn test_substitution_apply() {
    let mut subst = Substitution::new();
    let var = Type::var(1);
    let int_type = Type::primitive(PrimitiveType::Int);

    subst.bind(1, int_type.clone()).unwrap();

    let applied = subst.apply(&var);
    assert_eq!(applied, int_type);
  }

  #[test]
  fn test_substitution_compose() {
    let mut subst1 = Substitution::new();
    let mut subst2 = Substitution::new();

    subst1.bind(1, Type::primitive(PrimitiveType::Int)).unwrap();
    subst2
      .bind(2, Type::primitive(PrimitiveType::Bool))
      .unwrap();

    let composed = subst1.compose(&subst2);
    assert_eq!(composed.len(), 2);
  }

  #[test]
  fn test_occurs_check() {
    let mut subst = Substitution::new();
    let var1 = Type::var(1);
    let func = Type::function(vec![var1.clone()], var1);

    let result = subst.bind(1, func);
    assert!(result.is_err());

    if let Err(msg) = result {
      assert!(msg.contains("Occurs check failed"));
    } else {
      panic!("Expected occurs check error");
    }
  }

  #[test]
  fn test_generalize() {
    let mut inference = TypeInference::new();
    let var1 = inference.fresh_type_variable();
    let func_type = Type::function(vec![var1.clone()], Type::primitive(PrimitiveType::Int));

    let generalized = inference.generalize(&func_type);

    match generalized {
      Type::Polymorphic(poly) => {
        assert_eq!(poly.variables.len(), 1);
        assert_eq!(poly.variables[0].id, 0); // First generated variable
      }
      _ => panic!("Expected polymorphic type"),
    }
  }

  #[test]
  fn test_instantiate() {
    let mut inference = TypeInference::new();
    let var1 = Type::var(0);
    let func_type = Type::function(vec![var1.clone()], Type::primitive(PrimitiveType::Int));
    let poly_type = Type::polymorphic(vec![TypeVariable::new(0)], func_type);

    let instantiated = inference.instantiate(&poly_type);

    // The instantiated type should have fresh type variables
    match instantiated {
      Type::Function(func) => {
        match &func.parameters[0] {
          Type::Variable(var) => {
            assert_eq!(var.id, 0); // Should be the fresh variable
          }
          _ => panic!("Expected type variable"),
        }
      }
      _ => panic!("Expected function type"),
    }
  }

  #[test]
  fn test_generalize_and_instantiate_roundtrip() {
    let mut inference = TypeInference::new();
    let var1 = inference.fresh_type_variable();
    let func_type = Type::function(vec![var1.clone()], Type::primitive(PrimitiveType::Int));

    // Generalize the type
    let generalized = inference.generalize(&func_type);

    // Instantiate it back
    let instantiated = inference.instantiate(&generalized);

    // The instantiated type should be a function with a fresh type variable
    match instantiated {
      Type::Function(func) => {
        assert_eq!(func.parameters.len(), 1);
        assert!(matches!(func.parameters[0], Type::Variable(_)));
        assert_eq!(
          func.return_type.as_ref(),
          &Type::primitive(PrimitiveType::Int)
        );
      }
      _ => panic!("Expected function type"),
    }
  }
}
