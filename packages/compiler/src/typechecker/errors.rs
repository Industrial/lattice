//! Type checking errors for the Lattice language.
//!
//! This module defines error types that can occur during type checking
//! and type inference operations.

use crate::lexer::SourceLocation;
use crate::types::types::Type;
use codespan_reporting::diagnostic::{Diagnostic, Label};
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
  /// Undefined type
  UndefinedType {
    type_name: String,
    location: Option<SourceLocation>,
  },
  /// Undefined constructor
  UndefinedConstructor {
    constructor_name: String,
    type_name: String,
    location: Option<SourceLocation>,
  },
  /// Empty match expression
  EmptyMatchExpression {
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
      TypeCheckError::UndefinedType { type_name, location } => {
        write!(f, "Undefined type: {}", type_name)?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
      TypeCheckError::UndefinedConstructor {
        constructor_name,
        type_name,
        location,
      } => {
        write!(
          f,
          "Undefined constructor: {} for type {}",
          constructor_name, type_name
        )?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
      TypeCheckError::EmptyMatchExpression { location } => {
        write!(f, "Empty match expression")?;
        if let Some(loc) = location {
          write!(f, " at {}", loc)?;
        }
        Ok(())
      }
    }
  }
}

impl std::error::Error for TypeCheckError {}

impl TypeCheckError {
  /// Convert this error to a codespan Diagnostic for formatted reporting
  pub fn to_diagnostic(&self) -> Diagnostic<()> {
    match self {
      TypeCheckError::TypeMismatch {
        expected,
        actual,
        location,
        context,
      } => {
        let mut diagnostic = Diagnostic::error()
          .with_message("Type mismatch")
          .with_code("E0308");

        if let Some(loc) = location {
          diagnostic = diagnostic.with_labels(vec![Label::primary((), loc.offset..loc.offset + 1)
            .with_message(format!("expected `{}`, found `{}`", expected, actual))]);
        }

        if let Some(ctx) = context {
          diagnostic = diagnostic.with_notes(vec![format!("Context: {}", ctx)]);
        }

        // Add helpful suggestions
        let suggestions = self.get_type_mismatch_suggestions(expected, actual);
        if !suggestions.is_empty() {
          diagnostic = diagnostic.with_notes(suggestions);
        }

        diagnostic
      }
      TypeCheckError::UndefinedVariable { name, location } => {
        let mut diagnostic = Diagnostic::error()
          .with_message(format!("Cannot find variable `{}` in this scope", name))
          .with_code("E0425");

        if let Some(loc) = location {
          diagnostic = diagnostic
            .with_labels(vec![Label::primary((), loc.offset..loc.offset + 1)
              .with_message("not found in this scope")]);
        }

        // Add suggestions for common typos or similar names
        let suggestions = vec![
          "Try checking the spelling of the variable name".to_string(),
          "Ensure the variable is declared before use".to_string(),
          "Check if the variable is in scope at this location".to_string(),
        ];
        diagnostic = diagnostic.with_notes(suggestions);

        diagnostic
      }
      TypeCheckError::InferenceFailed { message, location } => {
        let mut diagnostic = Diagnostic::error()
          .with_message("Type inference failed")
          .with_code("E0282");

        if let Some(loc) = location {
          diagnostic = diagnostic.with_labels(vec![
            Label::primary((), loc.offset..loc.offset + 1).with_message(message.clone())
          ]);
        }

        diagnostic = diagnostic.with_notes(vec![
          "The compiler could not infer the type of this expression".to_string(),
          "Try adding explicit type annotations".to_string(),
        ]);

        diagnostic
      }
      TypeCheckError::UnificationError { message, location } => {
        let mut diagnostic = Diagnostic::error()
          .with_message("Type unification failed")
          .with_code("E0308");

        if let Some(loc) = location {
          diagnostic = diagnostic.with_labels(vec![
            Label::primary((), loc.offset..loc.offset + 1).with_message(message.clone())
          ]);
        }

        diagnostic
      }
      TypeCheckError::OccursCheck { message, location } => {
        let mut diagnostic = Diagnostic::error()
          .with_message("Occurs check failed: infinite type")
          .with_code("E0072");

        if let Some(loc) = location {
          diagnostic = diagnostic.with_labels(vec![
            Label::primary((), loc.offset..loc.offset + 1).with_message(message.clone())
          ]);
        }

        diagnostic = diagnostic.with_notes(vec![
          "This type cannot be unified because it would create an infinite type".to_string(),
          "Try adding explicit type annotations or restructuring your code".to_string(),
        ]);

        diagnostic
      }
      TypeCheckError::GeneralizationError { message, location } => {
        let mut diagnostic = Diagnostic::error()
          .with_message("Type generalization failed")
          .with_code("E0282");

        if let Some(loc) = location {
          diagnostic = diagnostic.with_labels(vec![
            Label::primary((), loc.offset..loc.offset + 1).with_message(message.clone())
          ]);
        }

        diagnostic
      }
      TypeCheckError::InstantiationError { message, location } => {
        let mut diagnostic = Diagnostic::error()
          .with_message("Type instantiation failed")
          .with_code("E0282");

        if let Some(loc) = location {
          diagnostic = diagnostic.with_labels(vec![
            Label::primary((), loc.offset..loc.offset + 1).with_message(message.clone())
          ]);
        }

        diagnostic
      }
      TypeCheckError::AnnotationMismatch {
        annotated_type,
        inferred_type,
        location,
      } => {
        let mut diagnostic = Diagnostic::error()
          .with_message("Type annotation mismatch")
          .with_code("E0308");

        if let Some(loc) = location {
          diagnostic = diagnostic.with_labels(vec![Label::primary((), loc.offset..loc.offset + 1)
            .with_message(format!(
              "annotated type `{}` does not match inferred type `{}`",
              annotated_type, inferred_type
            ))]);
        }

        diagnostic = diagnostic.with_notes(vec![
          "The explicit type annotation conflicts with the inferred type".to_string(),
          "Try removing the annotation or changing it to match the inferred type".to_string(),
        ]);

        diagnostic
      }
      TypeCheckError::UndefinedType { type_name, location } => {
        let mut diagnostic = Diagnostic::error()
          .with_message("Undefined type")
          .with_code("E0412");

        if let Some(loc) = location {
          diagnostic = diagnostic.with_labels(vec![Label::primary((), loc.offset..loc.offset + 1)
            .with_message(format!("type `{}` is not defined", type_name))]);
        }

        diagnostic = diagnostic.with_notes(vec![
          "Make sure the type is properly imported or defined".to_string(),
        ]);

        diagnostic
      }
      TypeCheckError::UndefinedConstructor {
        constructor_name,
        type_name,
        location,
      } => {
        let mut diagnostic = Diagnostic::error()
          .with_message("Undefined constructor")
          .with_code("E0425");

        if let Some(loc) = location {
          diagnostic = diagnostic.with_labels(vec![Label::primary((), loc.offset..loc.offset + 1)
            .with_message(format!("constructor `{}` is not defined for type `{}`", constructor_name, type_name))]);
        }

        diagnostic = diagnostic.with_notes(vec![
          "Make sure the constructor is properly defined for this type".to_string(),
        ]);

        diagnostic
      }
      TypeCheckError::EmptyMatchExpression { location } => {
        let mut diagnostic = Diagnostic::error()
          .with_message("Empty match expression")
          .with_code("E0004");

        if let Some(loc) = location {
          diagnostic = diagnostic.with_labels(vec![Label::primary((), loc.offset..loc.offset + 1)
            .with_message("match expression must have at least one arm")]);
        }

        diagnostic = diagnostic.with_notes(vec![
          "Add at least one match arm to handle the expression".to_string(),
        ]);

        diagnostic
      }
    }
  }

  /// Get helpful suggestions for type mismatch errors
  fn get_type_mismatch_suggestions(&self, expected: &Type, actual: &Type) -> Vec<String> {
    let mut suggestions = Vec::new();

    // Common type mismatch suggestions
    match (expected, actual) {
      (Type::Primitive(exp_prim), Type::Primitive(act_prim)) => {
        if exp_prim == act_prim {
          suggestions.push("Types are the same but with different representations".to_string());
        } else {
          suggestions.push(format!(
            "Try converting from {} to {} using an explicit cast",
            act_prim, exp_prim
          ));
        }
      }
      (Type::Function(_), Type::Function(_)) => {
        suggestions.push("Function types have different signatures".to_string());
        suggestions.push("Check the number and types of parameters".to_string());
        suggestions.push("Check the return type".to_string());
      }
      (Type::Variable(_), _) => {
        suggestions.push("Expected a type variable but found a concrete type".to_string());
        suggestions
          .push("Try using a type annotation or let the compiler infer the type".to_string());
      }
      (_, Type::Variable(_)) => {
        suggestions.push("Expected a concrete type but found a type variable".to_string());
        suggestions.push("Try adding explicit type annotations".to_string());
      }
      _ => {
        suggestions.push("Types are fundamentally incompatible".to_string());
        suggestions.push("Check if you meant to use a different expression".to_string());
      }
    }

    suggestions
  }

  /// Create a simple formatted error report
  pub fn format_simple_report(&self) -> String {
    format!("{}", self)
  }

  /// Get the error code for this error type
  pub fn error_code(&self) -> &'static str {
    match self {
      TypeCheckError::TypeMismatch { .. } => "E0308",
      TypeCheckError::UndefinedVariable { .. } => "E0425",
      TypeCheckError::InferenceFailed { .. } => "E0282",
      TypeCheckError::UnificationError { .. } => "E0308",
      TypeCheckError::OccursCheck { .. } => "E0072",
      TypeCheckError::GeneralizationError { .. } => "E0282",
      TypeCheckError::InstantiationError { .. } => "E0282",
      TypeCheckError::AnnotationMismatch { .. } => "E0308",
      TypeCheckError::UndefinedType { .. } => "E0412",
      TypeCheckError::UndefinedConstructor { .. } => "E0425",
      TypeCheckError::EmptyMatchExpression { .. } => "E0004",
    }
  }

  /// Get the severity level for this error
  pub fn severity(&self) -> codespan_reporting::diagnostic::Severity {
    codespan_reporting::diagnostic::Severity::Error
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::types::{PrimitiveType, Type};

  fn create_test_location() -> SourceLocation {
    SourceLocation {
      line: 1,
      column: 1,
      offset: 0,
    }
  }

  #[test]
  fn test_type_mismatch_error_diagnostic() {
    let expected = Type::primitive(PrimitiveType::Int);
    let actual = Type::primitive(PrimitiveType::String);
    let location = create_test_location();

    let error = TypeCheckError::TypeMismatch {
      expected: expected.clone(),
      actual: actual.clone(),
      location: Some(location.clone()),
      context: Some("in function call".to_string()),
    };

    let diagnostic = error.to_diagnostic();
    assert_eq!(
      diagnostic.severity,
      codespan_reporting::diagnostic::Severity::Error
    );
    assert_eq!(diagnostic.code, Some("E0308".to_string()));
    assert_eq!(diagnostic.message, "Type mismatch");
  }

  #[test]
  fn test_undefined_variable_error_diagnostic() {
    let location = create_test_location();

    let error = TypeCheckError::UndefinedVariable {
      name: "undefined_var".to_string(),
      location: Some(location),
    };

    let diagnostic = error.to_diagnostic();
    assert_eq!(
      diagnostic.severity,
      codespan_reporting::diagnostic::Severity::Error
    );
    assert_eq!(diagnostic.code, Some("E0425".to_string()));
    assert_eq!(
      diagnostic.message,
      "Cannot find variable `undefined_var` in this scope"
    );
  }

  #[test]
  fn test_occurs_check_error_diagnostic() {
    let location = create_test_location();

    let error = TypeCheckError::OccursCheck {
      message: "Type variable 'a' occurs in 'a -> a'".to_string(),
      location: Some(location),
    };

    let diagnostic = error.to_diagnostic();
    assert_eq!(
      diagnostic.severity,
      codespan_reporting::diagnostic::Severity::Error
    );
    assert_eq!(diagnostic.code, Some("E0072".to_string()));
    assert_eq!(diagnostic.message, "Occurs check failed: infinite type");
  }

  #[test]
  fn test_annotation_mismatch_error_diagnostic() {
    let annotated = Type::primitive(PrimitiveType::Int);
    let inferred = Type::primitive(PrimitiveType::String);
    let location = create_test_location();

    let error = TypeCheckError::AnnotationMismatch {
      annotated_type: annotated.clone(),
      inferred_type: inferred.clone(),
      location: Some(location),
    };

    let diagnostic = error.to_diagnostic();
    assert_eq!(
      diagnostic.severity,
      codespan_reporting::diagnostic::Severity::Error
    );
    assert_eq!(diagnostic.code, Some("E0308".to_string()));
    assert_eq!(diagnostic.message, "Type annotation mismatch");
  }

  #[test]
  fn test_error_codes() {
    let location = create_test_location();

    let type_mismatch = TypeCheckError::TypeMismatch {
      expected: Type::primitive(PrimitiveType::Int),
      actual: Type::primitive(PrimitiveType::String),
      location: Some(location.clone()),
      context: None,
    };
    assert_eq!(type_mismatch.error_code(), "E0308");

    let undefined_var = TypeCheckError::UndefinedVariable {
      name: "x".to_string(),
      location: Some(location.clone()),
    };
    assert_eq!(undefined_var.error_code(), "E0425");

    let occurs_check = TypeCheckError::OccursCheck {
      message: "test".to_string(),
      location: Some(location),
    };
    assert_eq!(occurs_check.error_code(), "E0072");
  }

  #[test]
  fn test_type_mismatch_suggestions() {
    let location = create_test_location();
    let error = TypeCheckError::TypeMismatch {
      expected: Type::primitive(PrimitiveType::Int),
      actual: Type::primitive(PrimitiveType::String),
      location: Some(location),
      context: None,
    };

    let suggestions = error.get_type_mismatch_suggestions(
      &Type::primitive(PrimitiveType::Int),
      &Type::primitive(PrimitiveType::String),
    );

    assert!(!suggestions.is_empty());
    assert!(suggestions.iter().any(|s| s.contains("Try converting")));
  }

  #[test]
  fn test_error_formatting() {
    let location = create_test_location();
    let error = TypeCheckError::UndefinedVariable {
      name: "test_var".to_string(),
      location: Some(location),
    };

    let formatted = error.format_simple_report();
    assert!(formatted.contains("Undefined variable"));
    assert!(formatted.contains("test_var"));
  }

  #[test]
  fn test_display_implementation() {
    let location = create_test_location();

    let type_mismatch = TypeCheckError::TypeMismatch {
      expected: Type::primitive(PrimitiveType::Int),
      actual: Type::primitive(PrimitiveType::String),
      location: Some(location.clone()),
      context: None,
    };

    let display_str = format!("{}", type_mismatch);
    assert!(display_str.contains("Type mismatch"));
    assert!(display_str.contains("Int"));
    assert!(display_str.contains("String"));

    let undefined_var = TypeCheckError::UndefinedVariable {
      name: "x".to_string(),
      location: Some(location),
    };

    let display_str = format!("{}", undefined_var);
    assert!(display_str.contains("Undefined variable"));
    assert!(display_str.contains("x"));
  }
}
