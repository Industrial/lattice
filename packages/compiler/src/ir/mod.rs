//! Intermediate Representation (IR) for the Lattice language.
//!
//! This module provides the data structures for representing Lattice programs
//! in A-normal form, which is suitable for optimization and code generation.
//!
//! # Modules
//!
//! - [`nodes`] - Core IR data structures (variables, literals, expressions, etc.)
//! - [`visitor`] - Visitor pattern for IR traversal and transformation
//! - [`converter`] - AST to IR conversion in A-normal form

pub mod converter;
pub mod nodes;
pub mod visitor;

pub use converter::*;
pub use nodes::*;
pub use visitor::*;
