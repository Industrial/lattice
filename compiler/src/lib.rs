//! Lattice Compiler
//!
//! This crate provides the compiler for the Lattice functional programming language.
//! It includes lexical analysis, parsing, type checking, and WASM code generation.

pub mod code_generator;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod type_checker;

pub use error::{CompilerError, CompilerResult};

/// Main compiler entry point
#[allow(dead_code)]
pub struct Compiler {
  config: CompilerConfig,
}

/// Compiler configuration
pub struct CompilerConfig {
  pub optimize: bool,
  pub debug_info: bool,
  pub target: CompilerTarget,
}

/// Compiler target
pub enum CompilerTarget {
  Wasm,
  Native,
}

impl Default for CompilerConfig {
  fn default() -> Self {
    Self {
      optimize: false,
      debug_info: true,
      target: CompilerTarget::Wasm,
    }
  }
}

impl Compiler {
  /// Create a new compiler with the given configuration
  pub fn new(config: CompilerConfig) -> Self {
    Self { config }
  }

  /// Compile source code to the target format
  pub fn compile(&self, _source: &str) -> CompilerResult<Vec<u8>> {
    // TODO: Implement compilation pipeline
    // TODO: Use self.config for compilation settings
    todo!("Compilation not yet implemented")
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_compiler_creation() {
    let config = CompilerConfig::default();
    let compiler = Compiler::new(config);
    assert!(matches!(compiler.config.target, CompilerTarget::Wasm));
  }
}
