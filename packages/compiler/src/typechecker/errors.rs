//! Type checking errors for the Lattice language.
//!
//! This module defines error types that can occur during type checking
//! and type inference operations.

use crate::lexer::SourceLocation;
use crate::types::types::Type;
use std::fmt;

/// Represents an error that occurred during type checking
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeCheckError {
  /// Type mismatch between expected and actual types
  TypeMismatch {
    expected: Type,
    actual: Type,
    location: Option<SourceLocation>,
    context: Option<String>,
  },
  /// Undefined variable or identifier
  UndefinedVariable {
    name: String,
    location: Option<SourceLocation>,
  },
  /// Type inference failed
  InferenceFailed {
    message: String,
    location: Option<SourceLocation>,
  },
  /// Unification error during type inference
  UnificationError {
    message: String,
    location: Option<SourceLocation>,
  },
  /// Occurs check failure (infinite type)
  OccursCheck {
    message: String,
    location: Option<SourceLocation>,
  },
  /// Generalization error
  GeneralizationError {
    message: String,
    location: Option<SourceLocation>,
  },
  /// Instantiation error
  InstantiationError {
    message: String,
    location: Option<SourceLocation>,
  },
  /// Explicit type annotation mismatch
  AnnotationMismatch {
    annotated_type: Type,
    inferred_type: Type,
    location: Option<SourceLocation>,
  },
}

impl fmt::Display for TypeCheckError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TypeCheckError::TypeMismatch {
        expected,
        actual,
        location,
        context,
      } => {
        write!(f, "Type mismatch: expected {}, got {}", expected, actual)?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        if let Some(ctx) = context {
          write!(f, " ({})", ctx)?;
        }
        Ok(())
      }
      TypeCheckError::UndefinedVariable { name, location } => {
        write!(f, "Undefined variable: {}", name)?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
      TypeCheckError::InferenceFailed { message, location } => {
        write!(f, "Type inference failed: {}", message)?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
      TypeCheckError::UnificationError { message, location } => {
        write!(f, "Unification error: {}", message)?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
      TypeCheckError::OccursCheck { message, location } => {
        write!(f, "Occurs check failed: {}", message)?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
      TypeCheckError::GeneralizationError { message, location } => {
        write!(f, "Generalization error: {}", message)?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
      TypeCheckError::InstantiationError { message, location } => {
        write!(f, "Instantiation error: {}", message)?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
      TypeCheckError::AnnotationMismatch {
        annotated_type,
        inferred_type,
        location,
      } => {
        write!(
          f,
          "Type annotation mismatch: annotated {}, inferred {}",
          annotated_type, inferred_type
        )?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
    }
  }
}

impl std::error::Error for TypeCheckError {}
