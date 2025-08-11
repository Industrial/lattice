//! Core functions for the Lattice language

use crate::types::{Bool, Float, Int};

/// Add two integers
pub fn add(a: &Int, b: &Int) -> Int {
  Int(a.0 + b.0)
}

/// Subtract two integers
pub fn sub(a: &Int, b: &Int) -> Int {
  Int(a.0 - b.0)
}

/// Multiply two integers
pub fn mul(a: &Int, b: &Int) -> Int {
  Int(a.0 * b.0)
}

/// Divide two integers
pub fn div(a: &Int, b: &Int) -> Option<Int> {
  if b.0 == 0 {
    None
  } else {
    Some(Int(a.0 / b.0))
  }
}

/// Check equality of two integers
pub fn eq(a: &Int, b: &Int) -> Bool {
  Bool(a.0 == b.0)
}

/// Check inequality of two integers
pub fn neq(a: &Int, b: &Int) -> Bool {
  Bool(a.0 != b.0)
}

/// Add two floats
pub fn add_float(a: &Float, b: &Float) -> Float {
  Float(a.0 + b.0)
}

/// Subtract two floats
pub fn sub_float(a: &Float, b: &Float) -> Float {
  Float(a.0 - b.0)
}

/// Multiply two floats
pub fn mul_float(a: &Float, b: &Float) -> Float {
  Float(a.0 * b.0)
}

/// Divide two floats
pub fn div_float(a: &Float, b: &Float) -> Option<Float> {
  if b.0 == 0.0 {
    None
  } else {
    Some(Float(a.0 / b.0))
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_integer_operations() {
    let a = Int(5);
    let b = Int(3);

    assert_eq!(add(&a, &b).0, 8);
    assert_eq!(sub(&a, &b).0, 2);
    assert_eq!(mul(&a, &b).0, 15);
    assert_eq!(div(&a, &b).unwrap().0, 1);
    assert_eq!(eq(&a, &b).0, false);
    assert_eq!(neq(&a, &b).0, true);
  }

  #[test]
  fn test_division_by_zero() {
    let a = Int(5);
    let b = Int(0);

    assert!(div(&a, &b).is_none());
  }
}
