//! Type annotator for the Lattice language.
//!
//! This module provides functionality to annotate AST nodes with
//! inferred type information using the Hindley-Milner type inference system.

// use crate::lexer::SourceLocation; // Temporarily unused
// use crate::parser::ast::{Expression, Statement, Identifier}; // Temporarily disabled due to parser issues
use crate::types::inference::TypeInference;
// use crate::types::types::{Type, PrimitiveType}; // Temporarily unused
// use std::collections::HashMap; // Temporarily unused
// use std::fmt; // Temporarily unused

use super::errors::TypeCheckError;

/// The main type annotator that performs type inference and annotation
#[derive(Debug)]
pub struct TypeAnnotator {
  /// Type inference engine
  inference: TypeInference,
  /// Error collection
  errors: Vec<TypeCheckError>,
}

impl TypeAnnotator {
  /// Create a new type annotator
  pub fn new() -> Self {
    Self {
      inference: TypeInference::new(),
      errors: Vec::new(),
    }
  }

  /// Annotate a program with type information
  /// TODO: This will be implemented once the parser is fixed
  pub fn annotate_program(&mut self, _program: &mut [()]) -> Result<(), Vec<TypeCheckError>> {
    // TODO: Implement program annotation
    // This will traverse the AST and annotate each node with its inferred type
    Ok(())
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
