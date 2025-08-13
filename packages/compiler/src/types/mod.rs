//! Type system for the Lattice language.
//!
//! This module provides the core type system data structures and functionality
//! for type checking and inference in the Lattice compiler.

// pub mod algorithm_w;
pub mod environment;
pub mod inference;
pub mod types;

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_module_imports() {
    // Test that all modules can be imported
    use environment::TypeEnvironment;
    use inference::TypeInference;
    use types::{PrimitiveType, Type};

    // Test that types can be instantiated
    let env = TypeEnvironment::new();
    let inference = TypeInference::new();
    let ty = Type::primitive(PrimitiveType::Int);

    assert!(matches!(env, TypeEnvironment));
    assert!(matches!(inference, TypeInference));
    assert!(matches!(ty, Type::Primitive(PrimitiveType::Int)));
  }

  #[test]
  fn test_module_structure() {
    // Test that the module structure is correct
    assert!(std::path::Path::new("src/types/environment.rs").exists());
    assert!(std::path::Path::new("src/types/inference.rs").exists());
    assert!(std::path::Path::new("src/types/types.rs").exists());
  }

  #[test]
  fn test_module_visibility() {
    // Test that all modules are public
    // This test ensures the module structure is accessible
    let _env = environment::TypeEnvironment::new();
    let _inference = inference::TypeInference::new();
    let _ty = types::Type::primitive(types::PrimitiveType::Int);
  }
}

// pub use algorithm_w::*;
pub use environment::*;
pub use inference::*;
pub use types::*;
