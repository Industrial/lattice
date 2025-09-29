//! Exhaustiveness checking for pattern matching in the Lattice type system.
//!
//! This module provides functionality to check whether pattern matching
//! expressions cover all possible cases of algebraic data types.

use super::types::{PrimitiveType, Type};
use std::collections::{HashMap, HashSet};

/// Represents a pattern for exhaustiveness checking
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
  /// Variable pattern (matches anything)
  Variable(String),
  /// Wildcard pattern (matches anything)
  Wildcard,
  /// Literal pattern (matches specific value)
  Literal(Type),
  /// Constructor pattern (matches specific constructor)
  Constructor {
    name: String,
    arguments: Vec<Pattern>,
  },
  /// Tuple pattern (matches tuple structure)
  Tuple(Vec<Pattern>),
  /// Or-pattern (matches either pattern)
  Or(Box<Pattern>, Box<Pattern>),
  /// As-pattern (matches pattern and binds to variable)
  As(Box<Pattern>, String),
}

/// Represents a pattern matrix for exhaustiveness checking
#[derive(Debug, Clone)]
pub struct PatternMatrix {
  /// Rows of patterns, where each row represents a match arm
  rows: Vec<Vec<Pattern>>,
  /// The types being matched against
  types: Vec<Type>,
}

impl PatternMatrix {
  /// Create a new pattern matrix
  pub fn new(types: Vec<Type>) -> Self {
    Self {
      rows: Vec::new(),
      types,
    }
  }

  /// Add a row of patterns to the matrix
  pub fn add_row(&mut self, patterns: Vec<Pattern>) {
    self.rows.push(patterns);
  }

  /// Check if the pattern matrix is exhaustive
  pub fn is_exhaustive(&self) -> bool {
    self.check_exhaustiveness(&self.types)
  }

  /// Get missing patterns that would make the matrix exhaustive
  pub fn get_missing_patterns(&self) -> Vec<Vec<Pattern>> {
    self.find_missing_patterns(&self.types)
  }

  /// Recursive exhaustiveness checking
  fn check_exhaustiveness(&self, types: &[Type]) -> bool {
    if types.is_empty() {
      return !self.rows.is_empty();
    }

    let first_type = &types[0];
    let remaining_types = &types[1..];

    match first_type {
      Type::Sum(sum_type) => {
        // Check if all variants are covered
        let covered_variants: HashSet<String> = self
          .rows
          .iter()
          .filter_map(|row| row.first())
          .filter_map(|pattern| self.get_constructor_name(pattern))
          .collect();

        let all_variants: HashSet<String> = sum_type
          .variants
          .iter()
          .map(|(name, _)| name.clone())
          .collect();

        // Check if all variants are covered
        if covered_variants == all_variants {
          // All variants are explicitly covered, continue checking remaining types
          self.check_exhaustiveness(remaining_types)
        } else {
          // Check if there are wildcard or variable patterns that would cover missing variants
          let has_wildcard = self.rows.iter().any(|row| {
            row.first().map_or(false, |pattern| {
              matches!(pattern, Pattern::Wildcard | Pattern::Variable(_))
            })
          });

          if has_wildcard {
            // Wildcard covers missing variants, continue checking remaining types
            self.check_exhaustiveness(remaining_types)
          } else {
            // Missing variants are not covered
            false
          }
        }
      }
      Type::Product(_product_type) => {
        // For product types, check if all fields are covered
        // This is more complex and would require checking tuple patterns
        // For now, assume exhaustive if there's at least one pattern
        !self.rows.is_empty() && self.check_exhaustiveness(remaining_types)
      }
      Type::Primitive(_) => {
        // For primitive types, check if there are wildcards or variables
        let has_wildcard = self.rows.iter().any(|row| {
          row.first().map_or(false, |pattern| {
            matches!(pattern, Pattern::Wildcard | Pattern::Variable(_))
          })
        });
        has_wildcard && self.check_exhaustiveness(remaining_types)
      }
      _ => {
        // For other types, assume exhaustive if there's at least one pattern
        !self.rows.is_empty() && self.check_exhaustiveness(remaining_types)
      }
    }
  }

  /// Find missing patterns that would make the matrix exhaustive
  fn find_missing_patterns(&self, types: &[Type]) -> Vec<Vec<Pattern>> {
    if types.is_empty() {
      return Vec::new();
    }

    let first_type = &types[0];
    let remaining_types = &types[1..];

    match first_type {
      Type::Sum(sum_type) => {
        let covered_variants: HashSet<String> = self
          .rows
          .iter()
          .filter_map(|row| row.first())
          .filter_map(|pattern| self.get_constructor_name(pattern))
          .collect();

        let all_variants: HashSet<String> = sum_type
          .variants
          .iter()
          .map(|(name, _)| name.clone())
          .collect();

        let missing_variants: HashSet<String> = all_variants
          .difference(&covered_variants)
          .cloned()
          .collect();

        if missing_variants.is_empty() {
          Vec::new()
        } else {
          // Generate missing patterns for uncovered variants
          let mut missing_patterns = Vec::new();
          for variant_name in missing_variants {
            let variant_pattern = Pattern::Constructor {
              name: variant_name,
              arguments: Vec::new(), // Simplified - would need proper argument patterns
            };
            let mut pattern_row = vec![variant_pattern];
            pattern_row.extend(vec![Pattern::Wildcard; remaining_types.len()]);
            missing_patterns.push(pattern_row);
          }
          missing_patterns
        }
      }
      _ => {
        // For non-sum types, no specific missing patterns
        Vec::new()
      }
    }
  }

  /// Extract constructor name from a pattern
  fn get_constructor_name(&self, pattern: &Pattern) -> Option<String> {
    match pattern {
      Pattern::Constructor { name, .. } => Some(name.clone()),
      _ => None,
    }
  }
}

/// Exhaustiveness checker for pattern matching
#[derive(Debug)]
pub struct ExhaustivenessChecker {
  /// Type environment for context
  type_env: HashMap<String, Type>,
}

impl ExhaustivenessChecker {
  /// Create a new exhaustiveness checker
  pub fn new() -> Self {
    Self {
      type_env: HashMap::new(),
    }
  }

  /// Add a type to the environment
  pub fn add_type(&mut self, name: String, ty: Type) {
    self.type_env.insert(name, ty);
  }

  /// Check if a match expression is exhaustive
  pub fn check_match_exhaustiveness(
    &self,
    matched_types: Vec<Type>,
    patterns: Vec<Vec<Pattern>>,
  ) -> ExhaustivenessResult {
    let mut matrix = PatternMatrix::new(matched_types);
    for pattern_row in patterns {
      matrix.add_row(pattern_row);
    }

    if matrix.is_exhaustive() {
      ExhaustivenessResult::Exhaustive
    } else {
      let missing_patterns = matrix.get_missing_patterns();
      ExhaustivenessResult::NonExhaustive { missing_patterns }
    }
  }

  /// Check exhaustiveness for a specific ADT
  pub fn check_adt_exhaustiveness(
    &self,
    adt_name: &str,
    patterns: Vec<Vec<Pattern>>,
  ) -> Option<ExhaustivenessResult> {
    if let Some(adt_type) = self.type_env.get(adt_name) {
      Some(self.check_match_exhaustiveness(vec![adt_type.clone()], patterns))
    } else {
      None
    }
  }
}

impl Default for ExhaustivenessChecker {
  fn default() -> Self {
    Self::new()
  }
}

/// Result of exhaustiveness checking
#[derive(Debug, Clone, PartialEq)]
pub enum ExhaustivenessResult {
  /// The pattern matching is exhaustive
  Exhaustive,
  /// The pattern matching is not exhaustive
  NonExhaustive {
    /// Patterns that are missing to make it exhaustive
    missing_patterns: Vec<Vec<Pattern>>,
  },
}

impl ExhaustivenessResult {
  /// Check if the result indicates exhaustiveness
  pub fn is_exhaustive(&self) -> bool {
    matches!(self, ExhaustivenessResult::Exhaustive)
  }

  /// Get missing patterns if any
  pub fn missing_patterns(&self) -> &[Vec<Pattern>] {
    match self {
      ExhaustivenessResult::Exhaustive => &[],
      ExhaustivenessResult::NonExhaustive { missing_patterns } => missing_patterns,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_pattern_matrix_creation() {
    let option_type = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);

    let matrix = PatternMatrix::new(vec![option_type]);
    assert_eq!(matrix.rows.len(), 0);
  }

  #[test]
  fn test_exhaustive_option_patterns() {
    let option_type = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);

    let mut matrix = PatternMatrix::new(vec![option_type]);

    // Add exhaustive patterns
    matrix.add_row(vec![Pattern::Constructor {
      name: "None".to_string(),
      arguments: Vec::new(),
    }]);
    matrix.add_row(vec![Pattern::Constructor {
      name: "Some".to_string(),
      arguments: Vec::new(),
    }]);

    assert!(matrix.is_exhaustive());
  }

  #[test]
  fn test_non_exhaustive_option_patterns() {
    let option_type = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);

    let mut matrix = PatternMatrix::new(vec![option_type]);

    // Add only one pattern
    matrix.add_row(vec![Pattern::Constructor {
      name: "None".to_string(),
      arguments: Vec::new(),
    }]);

    assert!(!matrix.is_exhaustive());
  }

  #[test]
  fn test_wildcard_exhaustiveness() {
    let option_type = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);

    let mut matrix = PatternMatrix::new(vec![option_type]);

    // Add wildcard pattern
    matrix.add_row(vec![Pattern::Wildcard]);

    assert!(matrix.is_exhaustive());
  }

  #[test]
  fn test_missing_patterns() {
    let option_type = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);

    let mut matrix = PatternMatrix::new(vec![option_type]);

    // Add only None pattern
    matrix.add_row(vec![Pattern::Constructor {
      name: "None".to_string(),
      arguments: Vec::new(),
    }]);

    let missing_patterns = matrix.get_missing_patterns();
    assert_eq!(missing_patterns.len(), 1);
    assert!(matches!(&missing_patterns[0][0], Pattern::Constructor { name, .. } if name == "Some"));
  }

  #[test]
  fn test_exhaustiveness_checker() {
    let mut checker = ExhaustivenessChecker::new();

    let option_type = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);

    checker.add_type("Option".to_string(), option_type);

    let patterns = vec![
      vec![Pattern::Constructor {
        name: "None".to_string(),
        arguments: Vec::new(),
      }],
      vec![Pattern::Constructor {
        name: "Some".to_string(),
        arguments: Vec::new(),
      }],
    ];

    let result = checker.check_match_exhaustiveness(
      vec![Type::sum(vec![
        ("None".to_string(), None),
        (
          "Some".to_string(),
          Some(Type::primitive(PrimitiveType::Int)),
        ),
      ])],
      patterns,
    );

    assert!(result.is_exhaustive());
  }

  #[test]
  fn test_adt_exhaustiveness_check() {
    let mut checker = ExhaustivenessChecker::new();

    let option_type = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);

    checker.add_type("Option".to_string(), option_type);

    let patterns = vec![vec![Pattern::Constructor {
      name: "None".to_string(),
      arguments: Vec::new(),
    }]];

    let result = checker.check_adt_exhaustiveness("Option", patterns);
    assert!(result.is_some());

    let result = result.unwrap();
    assert!(!result.is_exhaustive());
    assert!(!result.missing_patterns().is_empty());
  }
}
