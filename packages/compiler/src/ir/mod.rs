//! Intermediate Representation (IR) for the Lattice language.
//!
//! This module provides the data structures for representing Lattice programs
//! in A-normal form, which is suitable for optimization and code generation.

pub mod nodes;
pub mod visitor;

pub use nodes::*;
pub use visitor::*;
