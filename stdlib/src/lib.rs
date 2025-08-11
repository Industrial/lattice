//! Lattice Standard Library
//!
//! This crate provides the standard library for the Lattice functional programming language.
//! It includes core types, functions, and effect handlers.

pub mod effects;
pub mod functions;
pub mod types;

/// Standard library version
pub const VERSION: &str = "0.1.0";

/// Core types available in the standard library
pub mod prelude {
  pub use crate::effects::{Async, IO};
  pub use crate::functions::{add, div, eq, mul, neq, sub};
  pub use crate::types::{Bool, Float, Int, List, Option, Result, String};
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_version() {
    assert_eq!(VERSION, "0.1.0");
  }
}
