//! Type system for the Lattice language.
//!
//! This module provides the core type system data structures and functionality
//! for type checking and inference in the Lattice compiler.

pub mod environment;
pub mod inference;
pub mod types;

pub use environment::*;
pub use inference::*;
pub use types::*;
