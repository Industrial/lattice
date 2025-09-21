//! Type checker for the Lattice language.
//!
//! This module provides the type checking and inference functionality
//! for Lattice programs, including the integration between the AST
//! and the type inference system.

pub mod annotator;
pub mod errors;

pub use annotator::TypeAnnotator;
pub use errors::TypeCheckError;
