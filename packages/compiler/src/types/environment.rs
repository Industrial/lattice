//! Type environment for the Lattice language.
//!
//! This module provides a type environment structure that tracks bindings
//! of identifiers to types throughout the program scope, supporting
//! proper scoping rules and efficient type checking operations.

use super::types::{Type, TypeLocation};
use std::collections::HashMap;
use std::fmt;

/// Represents a type binding in the environment
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeBinding {
  pub ty: Type,
  pub location: Option<TypeLocation>,
  pub mutable: bool,
}

impl TypeBinding {
  pub fn new(ty: Type) -> Self {
    Self {
      ty,
      location: None,
      mutable: false,
    }
  }

  pub fn with_location(mut self, location: TypeLocation) -> Self {
    self.location = Some(location);
    self
  }

  pub fn with_mutability(mut self, mutable: bool) -> Self {
    self.mutable = mutable;
    self
  }
}

impl fmt::Display for TypeBinding {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.mutable {
      write!(f, "mut {}", self.ty)
    } else {
      write!(f, "{}", self.ty)
    }
  }
}

/// Represents a type environment that tracks identifier-to-type bindings
#[derive(Debug, Clone)]
pub struct TypeEnvironment {
  /// Current scope bindings
  bindings: HashMap<String, TypeBinding>,
  /// Parent environment for nested scopes
  parent: Option<Box<TypeEnvironment>>,
  /// Environment metadata
  metadata: EnvironmentMetadata,
}

/// Metadata about the environment
#[derive(Debug, Clone)]
pub struct EnvironmentMetadata {
  /// Environment name/identifier
  pub name: Option<String>,
  /// Whether this is a function scope
  pub is_function: bool,
  /// Whether this is a module scope
  pub is_module: bool,
  /// Whether this is a global scope
  pub is_global: bool,
}

impl EnvironmentMetadata {
  pub fn new() -> Self {
    Self {
      name: None,
      is_function: false,
      is_module: false,
      is_global: false,
    }
  }

  pub fn function(name: String) -> Self {
    Self {
      name: Some(name),
      is_function: true,
      is_module: false,
      is_global: false,
    }
  }

  pub fn module(name: String) -> Self {
    Self {
      name: Some(name),
      is_function: false,
      is_module: true,
      is_global: false,
    }
  }

  pub fn global() -> Self {
    Self {
      name: None,
      is_function: false,
      is_module: false,
      is_global: true,
    }
  }
}

impl TypeEnvironment {
  /// Create a new empty type environment
  pub fn new() -> Self {
    Self {
      bindings: HashMap::new(),
      parent: None,
      metadata: EnvironmentMetadata::new(),
    }
  }

  /// Create a new global type environment
  pub fn global() -> Self {
    Self {
      bindings: HashMap::new(),
      parent: None,
      metadata: EnvironmentMetadata::global(),
    }
  }

  /// Create a new function-scoped environment
  pub fn function(name: String) -> Self {
    Self {
      bindings: HashMap::new(),
      parent: None,
      metadata: EnvironmentMetadata::function(name),
    }
  }

  /// Create a new module-scoped environment
  pub fn module(name: String) -> Self {
    Self {
      bindings: HashMap::new(),
      parent: None,
      metadata: EnvironmentMetadata::module(name),
    }
  }

  /// Create a new child environment with the current as parent
  pub fn child(&self) -> Self {
    Self {
      bindings: HashMap::new(),
      parent: Some(Box::new(self.clone())),
      metadata: EnvironmentMetadata::new(),
    }
  }

  /// Create a new child environment with specific metadata
  pub fn child_with_metadata(&self, metadata: EnvironmentMetadata) -> Self {
    Self {
      bindings: HashMap::new(),
      parent: Some(Box::new(self.clone())),
      metadata,
    }
  }

  /// Add a type binding to the current environment
  pub fn bind(&mut self, identifier: String, ty: Type) -> Result<(), String> {
    if self.bindings.contains_key(&identifier) {
      Err(format!(
        "Identifier '{}' is already bound in this scope",
        identifier
      ))
    } else {
      self.bindings.insert(identifier, TypeBinding::new(ty));
      Ok(())
    }
  }

  /// Add a type binding with location information
  pub fn bind_with_location(
    &mut self,
    identifier: String,
    ty: Type,
    location: TypeLocation,
  ) -> Result<(), String> {
    if self.bindings.contains_key(&identifier) {
      Err(format!(
        "Identifier '{}' is already bound in this scope",
        identifier
      ))
    } else {
      let binding = TypeBinding::new(ty).with_location(location);
      self.bindings.insert(identifier, binding);
      Ok(())
    }
  }

  /// Add a mutable type binding
  pub fn bind_mutable(&mut self, identifier: String, ty: Type) -> Result<(), String> {
    if self.bindings.contains_key(&identifier) {
      Err(format!(
        "Identifier '{}' is already bound in this scope",
        identifier
      ))
    } else {
      let binding = TypeBinding::new(ty).with_mutability(true);
      self.bindings.insert(identifier, binding);
      Ok(())
    }
  }

  /// Look up a type binding by identifier
  pub fn lookup(&self, identifier: &str) -> Option<&TypeBinding> {
    // First check current scope
    if let Some(binding) = self.bindings.get(identifier) {
      return Some(binding);
    }

    // Then check parent scope
    if let Some(ref parent) = self.parent {
      return parent.lookup(identifier);
    }

    None
  }

  /// Look up a type binding by identifier, returning only the type
  pub fn lookup_type(&self, identifier: &str) -> Option<&Type> {
    self.lookup(identifier).map(|binding| &binding.ty)
  }

  /// Check if an identifier is bound in the current scope only
  pub fn is_bound_locally(&self, identifier: &str) -> bool {
    self.bindings.contains_key(identifier)
  }

  /// Check if an identifier is bound anywhere in the environment chain
  pub fn is_bound(&self, identifier: &str) -> bool {
    self.lookup(identifier).is_some()
  }

  /// Update an existing binding (only allowed in current scope)
  pub fn update(&mut self, identifier: &str, new_type: Type) -> Result<(), String> {
    if let Some(binding) = self.bindings.get_mut(identifier) {
      binding.ty = new_type;
      Ok(())
    } else {
      Err(format!(
        "Identifier '{}' is not bound in current scope",
        identifier
      ))
    }
  }

  /// Remove a binding from the current scope
  pub fn unbind(&mut self, identifier: &str) -> Result<TypeBinding, String> {
    self
      .bindings
      .remove(identifier)
      .ok_or_else(|| format!("Identifier '{}' is not bound in current scope", identifier))
  }

  /// Get all bindings in the current scope
  pub fn current_bindings(&self) -> &HashMap<String, TypeBinding> {
    &self.bindings
  }

  /// Get all bindings in the current scope as owned data
  pub fn current_bindings_owned(&self) -> HashMap<String, TypeBinding> {
    self.bindings.clone()
  }

  /// Get the depth of the environment chain
  pub fn depth(&self) -> usize {
    match &self.parent {
      Some(parent) => 1 + parent.depth(),
      None => 0,
    }
  }

  /// Check if this environment has a parent
  pub fn has_parent(&self) -> bool {
    self.parent.is_some()
  }

  /// Get the parent environment if it exists
  pub fn parent(&self) -> Option<&TypeEnvironment> {
    self.parent.as_deref()
  }

  /// Get the metadata for this environment
  pub fn metadata(&self) -> &EnvironmentMetadata {
    &self.metadata
  }

  /// Merge another environment into this one
  /// This creates a new environment with bindings from both
  pub fn merge(&self, other: &TypeEnvironment) -> Self {
    let mut merged = self.clone();

    // Add all bindings from the other environment
    for (identifier, binding) in &other.bindings {
      merged.bindings.insert(identifier.clone(), binding.clone());
    }

    merged
  }

  /// Create a new environment with additional bindings
  pub fn with_bindings<I>(&self, bindings: I) -> Self
  where
    I: IntoIterator<Item = (String, Type)>,
  {
    let mut new_env = self.clone();
    for (identifier, ty) in bindings {
      let _ = new_env.bind(identifier, ty);
    }
    new_env
  }

  /// Get all type variables used in this environment
  pub fn get_type_variables(&self) -> Vec<super::types::TypeVariable> {
    let mut vars = Vec::new();

    // Collect from current bindings
    for binding in self.bindings.values() {
      vars.extend(binding.ty.get_variables());
    }

    // Collect from parent environment
    if let Some(ref parent) = self.parent {
      vars.extend(parent.get_type_variables());
    }

    // Remove duplicates
    vars.sort_by_key(|v| v.id);
    vars.dedup_by_key(|v| v.id);
    vars
  }

  /// Check if the environment contains any type variables
  pub fn contains_type_variables(&self) -> bool {
    !self.get_type_variables().is_empty()
  }

  /// Get a summary of the environment contents
  pub fn summary(&self) -> EnvironmentSummary {
    let current_count = self.bindings.len();
    let depth = self.depth();
    let has_vars = self.contains_type_variables();

    EnvironmentSummary {
      current_bindings: current_count,
      depth,
      has_type_variables: has_vars,
      metadata: self.metadata.clone(),
    }
  }
}

/// Summary information about an environment
#[derive(Debug, Clone)]
pub struct EnvironmentSummary {
  pub current_bindings: usize,
  pub depth: usize,
  pub has_type_variables: bool,
  pub metadata: EnvironmentMetadata,
}

impl fmt::Display for EnvironmentSummary {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "Environment({} bindings, depth {}, vars: {})",
      self.current_bindings,
      self.depth,
      if self.has_type_variables { "yes" } else { "no" }
    )
  }
}

impl fmt::Display for TypeEnvironment {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "TypeEnvironment {{")?;

    if let Some(ref name) = self.metadata.name {
      write!(f, " name: {}", name)?;
    }

    write!(
      f,
      ", bindings: {}, depth: {} }}",
      self.bindings.len(),
      self.depth()
    )
  }
}

impl Default for TypeEnvironment {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::super::types::{PrimitiveType, Type};
  use super::*;

  #[test]
  fn test_type_environment_new() {
    let env = TypeEnvironment::new();
    assert_eq!(env.bindings.len(), 0);
    assert_eq!(env.depth(), 0);
    assert!(!env.has_parent());
  }

  #[test]
  fn test_type_environment_global() {
    let env = TypeEnvironment::global();
    assert!(env.metadata().is_global);
    assert!(!env.metadata().is_function);
    assert!(!env.metadata().is_module);
  }

  #[test]
  fn test_type_environment_function() {
    let env = TypeEnvironment::function("test_func".to_string());
    assert!(env.metadata().is_function);
    assert_eq!(env.metadata().name.as_ref().unwrap(), "test_func");
  }

  #[test]
  fn test_type_environment_module() {
    let env = TypeEnvironment::module("test_module".to_string());
    assert!(env.metadata().is_module);
    assert_eq!(env.metadata().name.as_ref().unwrap(), "test_module");
  }

  #[test]
  fn test_type_environment_bind_and_lookup() {
    let mut env = TypeEnvironment::new();
    let int_type = Type::primitive(PrimitiveType::Int);

    // Bind a type
    assert!(env.bind("x".to_string(), int_type.clone()).is_ok());

    // Look it up
    let found = env.lookup_type("x");
    assert!(found.is_some());
    assert_eq!(found.unwrap(), &int_type);
  }

  #[test]
  fn test_type_environment_duplicate_binding() {
    let mut env = TypeEnvironment::new();
    let int_type = Type::primitive(PrimitiveType::Int);
    let bool_type = Type::primitive(PrimitiveType::Bool);

    // First binding should succeed
    assert!(env.bind("x".to_string(), int_type).is_ok());

    // Second binding should fail
    assert!(env.bind("x".to_string(), bool_type).is_err());
  }

  #[test]
  fn test_type_environment_child_scope() {
    let mut parent = TypeEnvironment::new();
    let int_type = Type::primitive(PrimitiveType::Int);
    parent.bind("x".to_string(), int_type.clone()).unwrap();

    let child = parent.child();

    // Child should be able to see parent's binding
    assert!(child.is_bound("x"));
    assert_eq!(child.lookup_type("x").unwrap(), &int_type);

    // Child should not have the binding locally
    assert!(!child.is_bound_locally("x"));
  }

  #[test]
  fn test_type_environment_shadowing() {
    let mut parent = TypeEnvironment::new();
    let int_type = Type::primitive(PrimitiveType::Int);
    parent.bind("x".to_string(), int_type).unwrap();

    let mut child = parent.child();
    let bool_type = Type::primitive(PrimitiveType::Bool);
    child.bind("x".to_string(), bool_type.clone()).unwrap();

    // Child should see its own binding
    assert_eq!(child.lookup_type("x").unwrap(), &bool_type);
    assert!(child.is_bound_locally("x"));

    // Parent should still have its binding
    assert_eq!(
      parent.lookup_type("x").unwrap(),
      &Type::primitive(PrimitiveType::Int)
    );
  }

  #[test]
  fn test_type_environment_update() {
    let mut env = TypeEnvironment::new();
    let int_type = Type::primitive(PrimitiveType::Int);
    let bool_type = Type::primitive(PrimitiveType::Bool);

    env.bind("x".to_string(), int_type).unwrap();

    // Update should succeed
    assert!(env.update("x", bool_type.clone()).is_ok());
    assert_eq!(env.lookup_type("x").unwrap(), &bool_type);

    // Update non-existent should fail
    assert!(env.update("y", bool_type).is_err());
  }

  #[test]
  fn test_type_environment_unbind() {
    let mut env = TypeEnvironment::new();
    let int_type = Type::primitive(PrimitiveType::Int);

    env.bind("x".to_string(), int_type.clone()).unwrap();
    assert!(env.is_bound("x"));

    let binding = env.unbind("x").unwrap();
    assert_eq!(binding.ty, int_type);
    assert!(!env.is_bound("x"));

    // Unbinding non-existent should fail
    assert!(env.unbind("y").is_err());
  }

  #[test]
  fn test_type_environment_merge() {
    let mut env1 = TypeEnvironment::new();
    let mut env2 = TypeEnvironment::new();

    env1
      .bind("x".to_string(), Type::primitive(PrimitiveType::Int))
      .unwrap();
    env2
      .bind("y".to_string(), Type::primitive(PrimitiveType::Bool))
      .unwrap();

    let merged = env1.merge(&env2);

    assert!(merged.is_bound("x"));
    assert!(merged.is_bound("y"));
    assert_eq!(merged.bindings.len(), 2);
  }

  #[test]
  fn test_type_environment_with_bindings() {
    let env = TypeEnvironment::new();
    let bindings = vec![
      ("x".to_string(), Type::primitive(PrimitiveType::Int)),
      ("y".to_string(), Type::primitive(PrimitiveType::Bool)),
    ];

    let new_env = env.with_bindings(bindings);

    assert!(new_env.is_bound("x"));
    assert!(new_env.is_bound("y"));
    assert_eq!(new_env.bindings.len(), 2);
  }

  #[test]
  fn test_type_environment_depth() {
    let env1 = TypeEnvironment::new();
    let env2 = env1.child();
    let env3 = env2.child();

    assert_eq!(env1.depth(), 0);
    assert_eq!(env2.depth(), 1);
    assert_eq!(env3.depth(), 2);
  }

  #[test]
  fn test_type_environment_summary() {
    let mut env = TypeEnvironment::new();
    env
      .bind("x".to_string(), Type::primitive(PrimitiveType::Int))
      .unwrap();

    let summary = env.summary();
    assert_eq!(summary.current_bindings, 1);
    assert_eq!(summary.depth, 0);
    assert!(!summary.has_type_variables);
  }

  #[test]
  fn test_type_environment_display() {
    let env = TypeEnvironment::new();
    let display = env.to_string();
    assert!(display.contains("TypeEnvironment"));
    assert!(display.contains("bindings: 0"));
    assert!(display.contains("depth: 0"));
  }
}
