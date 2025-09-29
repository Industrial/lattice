//! Comprehensive tests for Algebraic Data Type (ADT) support in the Lattice type system.
//!
//! This module tests the type system's support for:
//! - Sum types (variants/enums)
//! - Product types (tuples and records)
//! - Type applications (generic types)
//! - Polymorphic types
//! - Type unification for ADTs
//! - Type inference for ADTs

use super::inference::{TypeInference, UnificationError};
use super::types::{PrimitiveType, Type, TypeVariable};

#[cfg(test)]
mod tests {
  use super::*;

  /// Test creating and displaying sum types (ADTs)
  #[test]
  fn test_sum_type_creation_and_display() {
    // Test simple enum-like sum type
    let option_type = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);

    assert_eq!(option_type.to_string(), "None | Some of Int");

    // Test more complex sum type with multiple variants
    let result_type = Type::sum(vec![
      (
        "Ok".to_string(),
        Some(Type::primitive(PrimitiveType::String)),
      ),
      (
        "Error".to_string(),
        Some(Type::primitive(PrimitiveType::String)),
      ),
    ]);

    assert_eq!(result_type.to_string(), "Ok of String | Error of String");

    // Test sum type with no payload variants
    let color_type = Type::sum(vec![
      ("Red".to_string(), None),
      ("Green".to_string(), None),
      ("Blue".to_string(), None),
    ]);

    assert_eq!(color_type.to_string(), "Red | Green | Blue");
  }

  /// Test creating and displaying product types (tuples and records)
  #[test]
  fn test_product_type_creation_and_display() {
    // Test tuple type
    let point_tuple = Type::tuple(vec![
      Type::primitive(PrimitiveType::Int),
      Type::primitive(PrimitiveType::Int),
    ]);

    assert_eq!(point_tuple.to_string(), "(Int × Int)");

    // Test record type
    let person_record = Type::record(vec![
      ("name".to_string(), Type::primitive(PrimitiveType::String)),
      ("age".to_string(), Type::primitive(PrimitiveType::Int)),
    ]);

    assert_eq!(person_record.to_string(), "{ name: String, age: Int }");

    // Test mixed tuple with different types
    let mixed_tuple = Type::tuple(vec![
      Type::primitive(PrimitiveType::String),
      Type::primitive(PrimitiveType::Bool),
      Type::primitive(PrimitiveType::Float),
    ]);

    assert_eq!(mixed_tuple.to_string(), "(String × Bool × Float)");
  }

  /// Test creating and displaying type applications (generic types)
  #[test]
  fn test_type_application_creation_and_display() {
    // Test simple generic type
    let list_of_int = Type::application(
      "List".to_string(),
      vec![Type::primitive(PrimitiveType::Int)],
    );

    assert_eq!(list_of_int.to_string(), "List<Int>");

    // Test generic type with multiple parameters
    let map_type = Type::application(
      "Map".to_string(),
      vec![
        Type::primitive(PrimitiveType::String),
        Type::primitive(PrimitiveType::Int),
      ],
    );

    assert_eq!(map_type.to_string(), "Map<String, Int>");

    // Test nested generic types
    let list_of_lists = Type::application(
      "List".to_string(),
      vec![Type::application(
        "List".to_string(),
        vec![Type::primitive(PrimitiveType::String)],
      )],
    );

    assert_eq!(list_of_lists.to_string(), "List<List<String>>");
  }

  /// Test polymorphic types (universally quantified types)
  #[test]
  fn test_polymorphic_type_creation_and_display() {
    // Test polymorphic identity function type
    let var_a = TypeVariable::new(0).with_name("a".to_string());
    let identity_type = Type::polymorphic(
      vec![var_a.clone()],
      Type::arrow(Type::Variable(var_a.clone()), Type::Variable(var_a)),
    );

    assert_eq!(identity_type.to_string(), "∀a. a -> a");

    // Test polymorphic list type
    let var_b = TypeVariable::new(1).with_name("b".to_string());
    let list_type = Type::polymorphic(
      vec![var_b.clone()],
      Type::application("List".to_string(), vec![Type::Variable(var_b)]),
    );

    assert_eq!(list_type.to_string(), "∀b. List<b>");
  }

  /// Test type unification for sum types
  #[test]
  fn test_sum_type_unification() {
    let mut inference = TypeInference::new();

    // Test unifying identical sum types
    let option1 = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);
    let option2 = option1.clone();

    let result = inference.unify(&option1, &option2);
    assert!(result.is_ok());

    // Test unifying sum types with type variables
    let var_a = inference.fresh_type_variable();
    let option_with_var = Type::sum(vec![
      ("None".to_string(), None),
      ("Some".to_string(), Some(var_a.clone())),
    ]);
    let option_with_int = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);

    let result = inference.unify(&option_with_var, &option_with_int);
    assert!(result.is_ok());

    // Test unifying incompatible sum types
    let result_type = Type::sum(vec![
      (
        "Ok".to_string(),
        Some(Type::primitive(PrimitiveType::String)),
      ),
      (
        "Error".to_string(),
        Some(Type::primitive(PrimitiveType::String)),
      ),
    ]);
    let option_type = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);

    let result = inference.unify(&result_type, &option_type);
    assert!(result.is_err());
    // The error could be TypeMismatch or VariantMismatch - both are valid
    let error = result.unwrap_err();
    assert!(matches!(
      error,
      UnificationError::TypeMismatch { .. } | UnificationError::VariantMismatch { .. }
    ));
  }

  /// Test type unification for product types
  #[test]
  fn test_product_type_unification() {
    let mut inference = TypeInference::new();

    // Test unifying identical tuple types
    let tuple1 = Type::tuple(vec![
      Type::primitive(PrimitiveType::Int),
      Type::primitive(PrimitiveType::String),
    ]);
    let tuple2 = tuple1.clone();

    let result = inference.unify(&tuple1, &tuple2);
    assert!(result.is_ok());

    // Test unifying tuple types with type variables
    let var_a = inference.fresh_type_variable();
    let var_b = inference.fresh_type_variable();
    let tuple_with_vars = Type::tuple(vec![var_a.clone(), var_b.clone()]);
    let tuple_with_types = Type::tuple(vec![
      Type::primitive(PrimitiveType::Int),
      Type::primitive(PrimitiveType::String),
    ]);

    let result = inference.unify(&tuple_with_vars, &tuple_with_types);
    assert!(result.is_ok());

    // Test unifying record types
    let record1 = Type::record(vec![
      ("name".to_string(), Type::primitive(PrimitiveType::String)),
      ("age".to_string(), Type::primitive(PrimitiveType::Int)),
    ]);
    let record2 = record1.clone();

    let result = inference.unify(&record1, &record2);
    assert!(result.is_ok());

    // Test unifying incompatible product types
    let tuple3 = Type::tuple(vec![Type::primitive(PrimitiveType::Int)]);
    let tuple4 = Type::tuple(vec![
      Type::primitive(PrimitiveType::Int),
      Type::primitive(PrimitiveType::String),
    ]);

    let result = inference.unify(&tuple3, &tuple4);
    assert!(result.is_err());
    assert!(matches!(
      result.unwrap_err(),
      UnificationError::ArityMismatch { .. }
    ));
  }

  /// Test type unification for type applications
  #[test]
  fn test_type_application_unification() {
    let mut inference = TypeInference::new();

    // Test unifying identical type applications
    let list1 = Type::application(
      "List".to_string(),
      vec![Type::primitive(PrimitiveType::Int)],
    );
    let list2 = list1.clone();

    let result = inference.unify(&list1, &list2);
    assert!(result.is_ok());

    // Test unifying type applications with type variables
    let var_a = inference.fresh_type_variable();
    let list_with_var = Type::application("List".to_string(), vec![var_a.clone()]);
    let list_with_int = Type::application(
      "List".to_string(),
      vec![Type::primitive(PrimitiveType::Int)],
    );

    let result = inference.unify(&list_with_var, &list_with_int);
    assert!(result.is_ok());

    // Test unifying incompatible type applications
    let list_type = Type::application(
      "List".to_string(),
      vec![Type::primitive(PrimitiveType::Int)],
    );
    let map_type = Type::application(
      "Map".to_string(),
      vec![Type::primitive(PrimitiveType::String)],
    );

    let result = inference.unify(&list_type, &map_type);
    assert!(result.is_err());
    assert!(matches!(
      result.unwrap_err(),
      UnificationError::TypeMismatch { .. }
    ));
  }

  /// Test polymorphic type generalization and instantiation
  #[test]
  fn test_polymorphic_type_operations() {
    let mut inference = TypeInference::new();

    // Test generalization
    let var_a = inference.fresh_type_variable();
    let var_b = inference.fresh_type_variable();
    let function_type = Type::function(vec![var_a.clone()], var_b.clone());

    let generalized = inference.generalize(&function_type);

    match &generalized {
      Type::Polymorphic(poly) => {
        assert_eq!(poly.variables.len(), 2);
        assert_eq!(poly.variables[0].id, 0);
        assert_eq!(poly.variables[1].id, 1);
      }
      _ => panic!("Expected polymorphic type"),
    }

    // Test instantiation
    let instantiated = inference.instantiate(&generalized);

    match instantiated {
      Type::Function(func) => {
        assert_eq!(func.parameters.len(), 1);
        assert!(matches!(func.parameters[0], Type::Variable(_)));
        assert!(matches!(*func.return_type, Type::Variable(_)));
      }
      _ => panic!("Expected function type"),
    }
  }

  /// Test complex ADT scenarios
  #[test]
  fn test_complex_adt_scenarios() {
    let mut inference = TypeInference::new();

    // Test nested ADTs: List<Option<Int>>
    let option_int = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);
    let list_option_int = Type::application("List".to_string(), vec![option_int]);

    // Test unifying with type variable
    let var_a = inference.fresh_type_variable();
    let list_var = Type::application("List".to_string(), vec![var_a]);

    let result = inference.unify(&list_option_int, &list_var);
    assert!(result.is_ok());

    // Test function type with ADT parameters and return type (including type variables)
    let var_a = inference.fresh_type_variable();
    let func_with_adt = Type::function(
      vec![
        Type::sum(vec![
          ("None".to_string(), None),
          ("Some".to_string(), Some(var_a.clone())),
        ]),
        Type::tuple(vec![
          Type::primitive(PrimitiveType::Int),
          Type::primitive(PrimitiveType::Bool),
        ]),
      ],
      Type::application("Result".to_string(), vec![var_a.clone()]),
    );

    // Test that the function type can be generalized
    let generalized_func = inference.generalize(&func_with_adt);
    assert!(matches!(generalized_func, Type::Polymorphic(_)));
  }

  /// Test type variable collection in ADTs
  #[test]
  fn test_type_variable_collection_in_adts() {
    // Test that type variables are correctly collected from complex ADTs
    let var_a = TypeVariable::new(0);
    let _var_b = TypeVariable::new(1);
    let var_c = TypeVariable::new(2);

    let complex_type = Type::function(
      vec![
        Type::sum(vec![
          ("None".to_string(), None),
          ("Some".to_string(), Some(Type::Variable(var_a.clone()))),
        ]),
        Type::tuple(vec![
          Type::Variable(_var_b.clone()),
          Type::application("List".to_string(), vec![Type::Variable(var_c.clone())]),
        ]),
      ],
      Type::Variable(var_a.clone()),
    );

    let collected_vars = complex_type.get_variables();
    assert_eq!(collected_vars.len(), 3);

    let var_ids: Vec<u64> = collected_vars.iter().map(|v| v.id).collect();
    assert!(var_ids.contains(&0));
    assert!(var_ids.contains(&1));
    assert!(var_ids.contains(&2));
  }

  /// Test substitution application to ADTs
  #[test]
  fn test_substitution_application_to_adts() {
    use super::super::inference::Substitution;

    let mut subst = Substitution::new();
    subst.bind(0, Type::primitive(PrimitiveType::Int)).unwrap();
    subst
      .bind(1, Type::primitive(PrimitiveType::String))
      .unwrap();

    let var_a = TypeVariable::new(0);
    let var_b = TypeVariable::new(1);

    let original_type = Type::sum(vec![
      ("None".to_string(), None),
      ("Some".to_string(), Some(Type::Variable(var_a))),
    ]);

    let substituted = subst.apply(&original_type);

    let expected = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);

    assert_eq!(substituted, expected);
  }

  /// Test error cases for ADT unification
  #[test]
  fn test_adt_unification_errors() {
    let mut inference = TypeInference::new();

    // Test variant name mismatch
    let option1 = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);
    let result1 = Type::sum(vec![
      ("Ok".to_string(), Some(Type::primitive(PrimitiveType::Int))),
      (
        "Error".to_string(),
        Some(Type::primitive(PrimitiveType::String)),
      ),
    ]);

    let result = inference.unify(&option1, &result1);
    assert!(result.is_err());
    assert!(matches!(
      result.unwrap_err(),
      UnificationError::VariantMismatch { .. }
    ));

    // Test variant count mismatch
    let option2 = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);
    let option3 = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
      ("Maybe".to_string(), None),
    ]);

    let result = inference.unify(&option2, &option3);
    assert!(result.is_err());
    assert!(matches!(
      result.unwrap_err(),
      UnificationError::ArityMismatch { .. }
    ));
  }
}
