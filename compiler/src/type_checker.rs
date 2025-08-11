//! Type checking for the Lattice language

use crate::error::CompilerResult;
use crate::parser::AstNode;

/// Type environment for type checking
pub struct TypeEnvironment {
  bindings: std::collections::HashMap<String, Type>,
}

impl TypeEnvironment {
  /// Create a new type environment
  pub fn new() -> Self {
    Self {
      bindings: std::collections::HashMap::new(),
    }
  }

  /// Bind a name to a type
  pub fn bind(&mut self, name: String, type_: Type) {
    self.bindings.insert(name, type_);
  }

  /// Look up a type binding
  pub fn lookup(&self, name: &str) -> Option<&Type> {
    self.bindings.get(name)
  }
}

/// Type representation
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
  Variable(String),
  Basic(String),
  Function(Box<Type>, Box<Type>),
  Generic(String, Vec<Type>),
  Effect(String, Vec<Type>),
}

/// Type checker for the Lattice language
#[allow(dead_code)]
pub struct TypeChecker {
  environment: TypeEnvironment,
}

impl TypeChecker {
  /// Create a new type checker
  pub fn new() -> Self {
    Self {
      environment: TypeEnvironment::new(),
    }
  }

  /// Type check a program
  pub fn check(&mut self, _program: &[AstNode]) -> CompilerResult<()> {
    // TODO: Implement actual type checking
    todo!("Type checking not yet implemented")
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_type_environment() {
    let mut env = TypeEnvironment::new();
    let int_type = Type::Basic("Int".to_string());
    env.bind("x".to_string(), int_type.clone());
    assert_eq!(env.lookup("x"), Some(&int_type));
  }
}
