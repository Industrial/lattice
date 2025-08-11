//! Code generation for the Lattice language

use crate::error::CompilerResult;
use crate::parser::AstNode;

/// Code generation target
pub enum Target {
  Wasm,
  Native,
}

/// WASM module configuration
pub struct WasmConfig {
  pub optimize: bool,
  pub debug_info: bool,
  pub features: Vec<String>,
}

impl Default for WasmConfig {
  fn default() -> Self {
    Self {
      optimize: false,
      debug_info: true,
      features: vec!["gc".to_string()],
    }
  }
}

/// Code generator for the Lattice language
#[allow(dead_code)]
pub struct CodeGenerator {
  target: Target,
  wasm_config: WasmConfig,
}

impl CodeGenerator {
  /// Create a new code generator
  pub fn new(target: Target) -> Self {
    Self {
      target,
      wasm_config: WasmConfig::default(),
    }
  }

  /// Generate code from AST
  pub fn generate(&self, ast: &[AstNode]) -> CompilerResult<Vec<u8>> {
    match self.target {
      Target::Wasm => self.generate_wasm(ast),
      Target::Native => self.generate_native(ast),
    }
  }

  /// Generate WASM code
  fn generate_wasm(&self, _ast: &[AstNode]) -> CompilerResult<Vec<u8>> {
    // TODO: Implement WASM code generation
    todo!("WASM code generation not yet implemented")
  }

  /// Generate native code
  fn generate_native(&self, _ast: &[AstNode]) -> CompilerResult<Vec<u8>> {
    // TODO: Implement native code generation
    todo!("Native code generation not yet implemented")
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_code_generator_creation() {
    let generator = CodeGenerator::new(Target::Wasm);
    assert!(matches!(generator.target, Target::Wasm));
  }
}
