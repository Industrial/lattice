//! Core type definitions for the Lattice language.
//!
//! This module provides the fundamental data structures for representing
//! Lattice's type system, including primitive types, function types,
//! type variables, algebraic data types, and effect types.

use std::fmt;

/// Represents a source location for type information
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeLocation {
  pub line: usize,
  pub column: usize,
  pub file: Option<String>,
}

impl TypeLocation {
  pub fn new(line: usize, column: usize) -> Self {
    Self {
      line,
      column,
      file: None,
    }
  }

  pub fn with_file(mut self, file: String) -> Self {
    self.file = Some(file);
    self
  }
}

impl fmt::Display for TypeLocation {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(ref file) = self.file {
      write!(f, "{}:{}:{}", file, self.line, self.column)
    } else {
      write!(f, "{}:{}", self.line, self.column)
    }
  }
}

/// Represents a type variable for polymorphic types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVariable {
  pub id: u64,
  pub name: Option<String>,
  pub location: Option<TypeLocation>,
}

impl TypeVariable {
  pub fn new(id: u64) -> Self {
    Self {
      id,
      name: None,
      location: None,
    }
  }

  pub fn with_name(mut self, name: String) -> Self {
    self.name = Some(name);
    self
  }

  pub fn with_location(mut self, location: TypeLocation) -> Self {
    self.location = Some(location);
    self
  }
}

impl fmt::Display for TypeVariable {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(ref name) = self.name {
      write!(f, "{}", name)
    } else {
      write!(f, "α{}", self.id)
    }
  }
}

/// Represents a primitive type in the Lattice language
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
  Int,
  Float,
  Bool,
  String,
  Char,
  Unit,
}

impl fmt::Display for PrimitiveType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      PrimitiveType::Int => write!(f, "Int"),
      PrimitiveType::Float => write!(f, "Float"),
      PrimitiveType::Bool => write!(f, "Bool"),
      PrimitiveType::String => write!(f, "String"),
      PrimitiveType::Char => write!(f, "Char"),
      PrimitiveType::Unit => write!(f, "Unit"),
    }
  }
}

/// Represents a function type with parameter and return types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionType {
  pub parameters: Vec<Type>,
  pub return_type: Box<Type>,
  pub location: Option<TypeLocation>,
}

impl FunctionType {
  pub fn new(parameters: Vec<Type>, return_type: Type) -> Self {
    Self {
      parameters,
      return_type: Box::new(return_type),
      location: None,
    }
  }

  pub fn with_location(mut self, location: TypeLocation) -> Self {
    self.location = Some(location);
    self
  }

  pub fn arrow(params: Vec<Type>, return_type: Type) -> Self {
    Self::new(params, return_type)
  }

  pub fn unary(param: Type, return_type: Type) -> Self {
    Self::new(vec![param], return_type)
  }
}

impl fmt::Display for FunctionType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.parameters.is_empty() {
      write!(f, "() -> {}", self.return_type)
    } else if self.parameters.len() == 1 {
      write!(f, "{} -> {}", self.parameters[0], self.return_type)
    } else {
      let params = self
        .parameters
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(" × ");
      write!(f, "({}) -> {}", params, self.return_type)
    }
  }
}

/// Represents a product type (tuple or record)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProductType {
  pub fields: Vec<(Option<String>, Type)>,
  pub location: Option<TypeLocation>,
}

impl ProductType {
  pub fn new(fields: Vec<(Option<String>, Type)>) -> Self {
    Self {
      fields,
      location: None,
    }
  }

  pub fn tuple(fields: Vec<Type>) -> Self {
    let fields = fields.into_iter().map(|t| (None, t)).collect();
    Self::new(fields)
  }

  pub fn record(fields: Vec<(String, Type)>) -> Self {
    let fields = fields
      .into_iter()
      .map(|(name, t)| (Some(name), t))
      .collect();
    Self::new(fields)
  }

  pub fn with_location(mut self, location: TypeLocation) -> Self {
    self.location = Some(location);
    self
  }
}

impl fmt::Display for ProductType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.fields.iter().all(|(name, _)| name.is_none()) {
      // Tuple type
      let fields = self
        .fields
        .iter()
        .map(|(_, t)| t.to_string())
        .collect::<Vec<_>>()
        .join(" × ");
      write!(f, "({})", fields)
    } else {
      // Record type
      let fields = self
        .fields
        .iter()
        .map(|(name, t)| {
          if let Some(ref name) = name {
            format!("{}: {}", name, t)
          } else {
            t.to_string()
          }
        })
        .collect::<Vec<_>>()
        .join(", ");
      write!(f, "{{ {} }}", fields)
    }
  }
}

/// Represents a sum type (variant or enum)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SumType {
  pub variants: Vec<(String, Option<Type>)>,
  pub location: Option<TypeLocation>,
}

impl SumType {
  pub fn new(variants: Vec<(String, Option<Type>)>) -> Self {
    Self {
      variants,
      location: None,
    }
  }

  pub fn with_location(mut self, location: TypeLocation) -> Self {
    self.location = Some(location);
    self
  }
}

impl fmt::Display for SumType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let variants = self
      .variants
      .iter()
      .map(|(name, ty)| {
        if let Some(ref ty) = ty {
          format!("{} of {}", name, ty)
        } else {
          name.clone()
        }
      })
      .collect::<Vec<_>>()
      .join(" | ");
    write!(f, "{}", variants)
  }
}

/// Represents an effect type for the effect system
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EffectType {
  pub effect_name: String,
  pub parameters: Vec<Type>,
  pub location: Option<TypeLocation>,
}

impl EffectType {
  pub fn new(effect_name: String, parameters: Vec<Type>) -> Self {
    Self {
      effect_name,
      parameters,
      location: None,
    }
  }

  pub fn with_location(mut self, location: TypeLocation) -> Self {
    self.location = Some(location);
    self
  }
}

impl fmt::Display for EffectType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.parameters.is_empty() {
      write!(f, "effect {}", self.effect_name)
    } else {
      let params = self
        .parameters
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(", ");
      write!(f, "effect {}<{}>", self.effect_name, params)
    }
  }
}

/// Represents a type application (e.g., List<Int>)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeApplication {
  pub constructor: String,
  pub arguments: Vec<Type>,
  pub location: Option<TypeLocation>,
}

impl TypeApplication {
  pub fn new(constructor: String, arguments: Vec<Type>) -> Self {
    Self {
      constructor,
      arguments,
      location: None,
    }
  }

  pub fn with_location(mut self, location: TypeLocation) -> Self {
    self.location = Some(location);
    self
  }
}

impl fmt::Display for TypeApplication {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.arguments.is_empty() {
      write!(f, "{}", self.constructor)
    } else {
      let args = self
        .arguments
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<_>>()
        .join(", ");
      write!(f, "{}<{}>", self.constructor, args)
    }
  }
}

/// Represents a reference type (mutable or immutable)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ReferenceType {
  pub inner_type: Box<Type>,
  pub mutable: bool,
  pub location: Option<TypeLocation>,
}

impl ReferenceType {
  pub fn new(inner_type: Type, mutable: bool) -> Self {
    Self {
      inner_type: Box::new(inner_type),
      mutable,
      location: None,
    }
  }

  pub fn immutable(inner_type: Type) -> Self {
    Self::new(inner_type, false)
  }

  pub fn mutable(inner_type: Type) -> Self {
    Self::new(inner_type, true)
  }

  pub fn with_location(mut self, location: TypeLocation) -> Self {
    self.location = Some(location);
    self
  }
}

impl fmt::Display for ReferenceType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if self.mutable {
      write!(f, "&mut {}", self.inner_type)
    } else {
      write!(f, "&{}", self.inner_type)
    }
  }
}

/// Represents a type with a constraint (e.g., Int where Int > 0)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstrainedType {
  pub base_type: Box<Type>,
  pub constraint: String,
  pub location: Option<TypeLocation>,
}

impl ConstrainedType {
  pub fn new(base_type: Type, constraint: String) -> Self {
    Self {
      base_type: Box::new(base_type),
      constraint,
      location: None,
    }
  }

  pub fn with_location(mut self, location: TypeLocation) -> Self {
    self.location = Some(location);
    self
  }
}

impl fmt::Display for ConstrainedType {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} where {}", self.base_type, self.constraint)
  }
}

/// The main type enum that represents all possible types in Lattice
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
  /// A type variable for polymorphic types
  Variable(TypeVariable),
  /// A primitive type
  Primitive(PrimitiveType),
  /// A function type
  Function(FunctionType),
  /// A product type (tuple or record)
  Product(ProductType),
  /// A sum type (variant or enum)
  Sum(SumType),
  /// An effect type
  Effect(EffectType),
  /// A type application
  Application(TypeApplication),
  /// A reference type
  Reference(ReferenceType),
  /// A constrained type
  Constrained(ConstrainedType),
  /// A type alias
  Alias(String, Box<Type>),
  /// A generic type
  Generic(String, Vec<Type>),
}

impl Type {
  /// Create a new type variable
  pub fn var(id: u64) -> Self {
    Type::Variable(TypeVariable::new(id))
  }

  /// Create a new type variable with a name
  pub fn named_var(id: u64, name: String) -> Self {
    Type::Variable(TypeVariable::new(id).with_name(name))
  }

  /// Create a new primitive type
  pub fn primitive(prim: PrimitiveType) -> Self {
    Type::Primitive(prim)
  }

  /// Create a new function type
  pub fn function(parameters: Vec<Type>, return_type: Type) -> Self {
    Type::Function(FunctionType::new(parameters, return_type))
  }

  /// Create a new arrow type (unary function)
  pub fn arrow(param: Type, return_type: Type) -> Self {
    Type::Function(FunctionType::unary(param, return_type))
  }

  /// Create a new tuple type
  pub fn tuple(fields: Vec<Type>) -> Self {
    Type::Product(ProductType::tuple(fields))
  }

  /// Create a new record type
  pub fn record(fields: Vec<(String, Type)>) -> Self {
    Type::Product(ProductType::record(fields))
  }

  /// Create a new sum type
  pub fn sum(variants: Vec<(String, Option<Type>)>) -> Self {
    Type::Sum(SumType::new(variants))
  }

  /// Create a new effect type
  pub fn effect(name: String, parameters: Vec<Type>) -> Self {
    Type::Effect(EffectType::new(name, parameters))
  }

  /// Create a new type application
  pub fn application(constructor: String, arguments: Vec<Type>) -> Self {
    Type::Application(TypeApplication::new(constructor, arguments))
  }

  /// Create a new immutable reference type
  pub fn reference(inner_type: Type) -> Self {
    Type::Reference(ReferenceType::immutable(inner_type))
  }

  /// Create a new mutable reference type
  pub fn mutable_reference(inner_type: Type) -> Self {
    Type::Reference(ReferenceType::mutable(inner_type))
  }

  /// Create a new constrained type
  pub fn constrained(base_type: Type, constraint: String) -> Self {
    Type::Constrained(ConstrainedType::new(base_type, constraint))
  }

  /// Create a new type alias
  pub fn alias(name: String, base_type: Type) -> Self {
    Type::Alias(name, Box::new(base_type))
  }

  /// Create a new generic type
  pub fn generic(name: String, parameters: Vec<Type>) -> Self {
    Type::Generic(name, parameters)
  }

  /// Get the location of this type if available
  pub fn location(&self) -> Option<&TypeLocation> {
    match self {
      Type::Variable(v) => v.location.as_ref(),
      Type::Function(f) => f.location.as_ref(),
      Type::Product(p) => p.location.as_ref(),
      Type::Sum(s) => s.location.as_ref(),
      Type::Effect(e) => e.location.as_ref(),
      Type::Application(a) => a.location.as_ref(),
      Type::Reference(r) => r.location.as_ref(),
      Type::Constrained(c) => c.location.as_ref(),
      _ => None,
    }
  }

  /// Check if this type contains any type variables
  pub fn contains_variables(&self) -> bool {
    match self {
      Type::Variable(_) => true,
      Type::Function(f) => {
        f.parameters.iter().any(|t| t.contains_variables()) || f.return_type.contains_variables()
      }
      Type::Product(p) => p.fields.iter().any(|(_, t)| t.contains_variables()),
      Type::Sum(s) => s
        .variants
        .iter()
        .any(|(_, t)| t.as_ref().map_or(false, |t| t.contains_variables())),
      Type::Effect(e) => e.parameters.iter().any(|t| t.contains_variables()),
      Type::Application(a) => a.arguments.iter().any(|t| t.contains_variables()),
      Type::Reference(r) => r.inner_type.contains_variables(),
      Type::Constrained(c) => c.base_type.contains_variables(),
      Type::Alias(_, t) => t.contains_variables(),
      Type::Generic(_, params) => params.iter().any(|t| t.contains_variables()),
      _ => false,
    }
  }

  /// Get all type variables in this type
  pub fn get_variables(&self) -> Vec<TypeVariable> {
    let mut vars = Vec::new();
    self.collect_variables(&mut vars);
    vars
  }

  /// Helper method to collect type variables
  fn collect_variables(&self, vars: &mut Vec<TypeVariable>) {
    match self {
      Type::Variable(v) => {
        if !vars.iter().any(|existing| existing.id == v.id) {
          vars.push(v.clone());
        }
      }
      Type::Function(f) => {
        for param in &f.parameters {
          param.collect_variables(vars);
        }
        f.return_type.collect_variables(vars);
      }
      Type::Product(p) => {
        for (_, t) in &p.fields {
          t.collect_variables(vars);
        }
      }
      Type::Sum(s) => {
        for (_, t) in &s.variants {
          if let Some(t) = t {
            t.collect_variables(vars);
          }
        }
      }
      Type::Effect(e) => {
        for param in &e.parameters {
          param.collect_variables(vars);
        }
      }
      Type::Application(a) => {
        for arg in &a.arguments {
          arg.collect_variables(vars);
        }
      }
      Type::Reference(r) => r.inner_type.collect_variables(vars),
      Type::Constrained(c) => c.base_type.collect_variables(vars),
      Type::Alias(_, t) => t.collect_variables(vars),
      Type::Generic(_, params) => {
        for param in params {
          param.collect_variables(vars);
        }
      }
      _ => {}
    }
  }
}

impl fmt::Display for Type {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Type::Variable(v) => write!(f, "{}", v),
      Type::Primitive(p) => write!(f, "{}", p),
      Type::Function(func) => write!(f, "{}", func),
      Type::Product(p) => write!(f, "{}", p),
      Type::Sum(s) => write!(f, "{}", s),
      Type::Effect(e) => write!(f, "{}", e),
      Type::Application(a) => write!(f, "{}", a),
      Type::Reference(r) => write!(f, "{}", r),
      Type::Constrained(c) => write!(f, "{}", c),
      Type::Alias(name, base) => write!(f, "{} = {}", name, base),
      Type::Generic(name, params) => {
        if params.is_empty() {
          write!(f, "{}", name)
        } else {
          let params_str = params
            .iter()
            .map(|t| t.to_string())
            .collect::<Vec<_>>()
            .join(", ");
          write!(f, "{}<{}>", name, params_str)
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_primitive_types() {
    let int_type = Type::primitive(PrimitiveType::Int);
    let float_type = Type::primitive(PrimitiveType::Float);
    let bool_type = Type::primitive(PrimitiveType::Bool);

    assert_eq!(int_type.to_string(), "Int");
    assert_eq!(float_type.to_string(), "Float");
    assert_eq!(bool_type.to_string(), "Bool");
  }

  #[test]
  fn test_function_types() {
    let func_type = Type::arrow(
      Type::primitive(PrimitiveType::Int),
      Type::primitive(PrimitiveType::Bool),
    );

    assert_eq!(func_type.to_string(), "Int -> Bool");
  }

  #[test]
  fn test_tuple_types() {
    let tuple_type = Type::tuple(vec![
      Type::primitive(PrimitiveType::Int),
      Type::primitive(PrimitiveType::String),
    ]);

    assert_eq!(tuple_type.to_string(), "(Int × String)");
  }

  #[test]
  fn test_record_types() {
    let record_type = Type::record(vec![
      ("x".to_string(), Type::primitive(PrimitiveType::Int)),
      ("y".to_string(), Type::primitive(PrimitiveType::String)),
    ]);

    assert_eq!(record_type.to_string(), "{ x: Int, y: String }");
  }

  #[test]
  fn test_sum_types() {
    let sum_type = Type::sum(vec![
      ("None".to_string(), None),
      (
        "Some".to_string(),
        Some(Type::primitive(PrimitiveType::Int)),
      ),
    ]);

    assert_eq!(sum_type.to_string(), "None | Some of Int");
  }

  #[test]
  fn test_type_variables() {
    let var_type = Type::named_var(1, "T".to_string());
    assert_eq!(var_type.to_string(), "T");
    assert!(var_type.contains_variables());
  }

  #[test]
  fn test_type_application() {
    let list_type = Type::application(
      "List".to_string(),
      vec![Type::primitive(PrimitiveType::Int)],
    );

    assert_eq!(list_type.to_string(), "List<Int>");
  }

  #[test]
  fn test_reference_types() {
    let ref_type = Type::reference(Type::primitive(PrimitiveType::Int));
    let mut_ref_type = Type::mutable_reference(Type::primitive(PrimitiveType::Int));

    assert_eq!(ref_type.to_string(), "&Int");
    assert_eq!(mut_ref_type.to_string(), "&mut Int");
  }

  #[test]
  fn test_constrained_types() {
    let constrained_type =
      Type::constrained(Type::primitive(PrimitiveType::Int), "Int > 0".to_string());

    assert_eq!(constrained_type.to_string(), "Int where Int > 0");
  }

  #[test]
  fn test_type_variable_collection() {
    let func_type = Type::arrow(Type::var(1), Type::arrow(Type::var(2), Type::var(1)));

    let vars = func_type.get_variables();
    assert_eq!(vars.len(), 2);
    assert!(vars.iter().any(|v| v.id == 1));
    assert!(vars.iter().any(|v| v.id == 2));
  }

  #[test]
  fn test_complex_type_display() {
    let complex_type = Type::function(
      vec![
        Type::tuple(vec![
          Type::primitive(PrimitiveType::Int),
          Type::primitive(PrimitiveType::String),
        ]),
        Type::var(1),
      ],
      Type::primitive(PrimitiveType::Bool),
    );

    // The display should be readable and properly formatted
    let display = complex_type.to_string();
    assert!(display.contains("Int"));
    assert!(display.contains("String"));
    assert!(display.contains("Bool"));
    assert!(display.contains("->"));
  }
}
