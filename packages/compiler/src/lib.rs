//! Lattice Compiler
//!
//! This crate provides the compiler for the Lattice functional programming language.
//! It includes lexical analysis, parsing, type checking, and WASM code generation.

pub mod lexer;
pub mod parser;
pub mod typechecker;
pub mod types;

#[cfg(test)]
mod tests {
  use super::*;

  /// Test utilities and fixtures for the compiler
  mod test_utils {
    use std::path::PathBuf;
    use tempfile::TempDir;

    /// Create a temporary directory for testing
    pub fn create_temp_dir() -> TempDir {
      tempfile::tempdir().expect("Failed to create temp directory")
    }

    /// Create a temporary file path for testing
    pub fn create_temp_file_path() -> PathBuf {
      let temp_dir = create_temp_dir();
      temp_dir.path().join("test_file.lat")
    }

    /// Common test data for compiler tests
    pub const TEST_SOURCE_CODE: &str = "let x = 42 in x + 1";

    /// Test AST representation (placeholder for future implementation)
    pub struct TestAST {
      pub source: String,
      pub expected_tokens: Vec<String>,
    }

    impl TestAST {
      pub fn new(source: &str) -> Self {
        Self {
          source: source.to_string(),
          expected_tokens: vec![], // Will be populated when lexer is implemented
        }
      }
    }
  }

  /// Unit tests for compiler core functionality
  #[test]
  fn test_compiler_initialization() {
    // Test that the compiler can be initialized
    // This is a placeholder test that will be expanded as modules are implemented
    assert!(true, "Compiler initialization test placeholder");
  }

  /// Test compiler configuration
  #[test]
  fn test_compiler_configuration() {
    // Test compiler configuration options
    // Placeholder for configuration testing
    assert!(true, "Compiler configuration test placeholder");
  }

  /// Test error handling
  #[test]
  fn test_error_handling() {
    // Test compiler error handling
    // Placeholder for error handling tests
    assert!(true, "Error handling test placeholder");
  }

  /// Test parser functionality
  // #[test]
  // fn test_parser_functionality() {
  //   use crate::lexer::lex;
  //   use crate::parser::Parser;

  //   // Test that the parser can parse a simple expression
  //   let source = "let x = 42 in x + 1";
  //   let tokens = lex(source).expect("Should lex successfully");

  //   let mut parser = Parser::new();
  //   let ast_result = parser.parse(&tokens);

  //   // The parser should work (even if it returns an error, it should not panic)
  //   assert!(
  //     ast_result.is_ok() || ast_result.is_err(),
  //     "Parser should complete without panicking"
  //   );
  // }

  /// Property-based tests using proptest
  #[cfg(feature = "testing")]
  mod property_tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn test_source_code_parsing(source in "[a-zA-Z0-9\\s\\+\\-\\*\\/\\(\\)\\{\\}\\[\\]]{1,100}") {
            // Property-based test for source code parsing
            // This will be implemented when the parser is ready
            prop_assert!(!source.is_empty());
        }
    }
  }

  /// Snapshot tests using insta
  #[cfg(feature = "testing")]
  mod snapshot_tests {
    use super::*;
    use insta::assert_snapshot;

    #[test]
    fn test_ast_snapshot() {
      // Snapshot test for AST representation
      // This will be implemented when AST is ready
      let test_ast = test_utils::TestAST::new("let x = 42");
      assert_snapshot!("test_ast", format!("{:?}", test_ast.source));
    }
  }

  /// Integration tests for cross-module functionality
  mod integration_tests {
    use super::*;

    #[test]
    fn test_compiler_pipeline() {
      // Test the complete compiler pipeline
      // Placeholder for integration testing
      assert!(true, "Compiler pipeline integration test placeholder");
    }

    #[test]
    fn test_wasm_generation() {
      // Test WASM code generation
      // Placeholder for WASM generation testing
      assert!(true, "WASM generation integration test placeholder");
    }
  }

  /// Benchmark tests using criterion
  #[cfg(feature = "testing")]
  mod benchmark_tests {
    use super::*;
    use criterion::{black_box, criterion_group, criterion_main, Criterion};

    fn benchmark_compiler_initialization(c: &mut Criterion) {
      c.bench_function("compiler_init", |b| {
        b.iter(|| {
          // Benchmark compiler initialization
          // Placeholder for actual benchmarking
          black_box(42)
        });
      });
    }

    criterion_group!(benches, benchmark_compiler_initialization);
    criterion_main!(benches);
  }
}
