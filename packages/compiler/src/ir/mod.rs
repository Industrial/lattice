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
//! - [`optimizations`] - Optimization passes (constant folding, DCE, inlining)

pub mod converter;
pub mod nodes;
pub mod optimizations;
pub mod visitor;

pub use converter::*;
pub use nodes::*;
pub use optimizations::*;
pub use visitor::*;
