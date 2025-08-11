//! Core types for the Lattice language

use serde::{Deserialize, Serialize};

/// Boolean type
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Bool(pub bool);

/// Integer type
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Int(pub i64);

/// Float type
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Float(pub f64);

/// String type
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct String(pub std::string::String);

/// List type
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum List<T> {
  Nil,
  Cons(T, Box<List<T>>),
}

/// Option type
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Option<T> {
  None,
  Some(T),
}

/// Result type
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Result<T, E> {
  Ok(T),
  Err(E),
}

impl<T> List<T> {
  /// Create an empty list
  pub fn empty() -> Self {
    List::Nil
  }

  /// Add an element to the front of the list
  pub fn cons(head: T, tail: List<T>) -> Self {
    List::Cons(head, Box::new(tail))
  }

  /// Check if the list is empty
  pub fn is_empty(&self) -> bool {
    matches!(self, List::Nil)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_list_operations() {
    let list = List::cons(1, List::cons(2, List::empty()));
    assert!(!list.is_empty());

    let empty = List::<i32>::empty();
    assert!(empty.is_empty());
  }
}
