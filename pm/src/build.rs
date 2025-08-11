//! Build system for Lattice packages

use crate::package::Package;

/// Build configuration
pub struct BuildConfig {
  pub target: BuildTarget,
  pub optimize: bool,
  pub debug: bool,
}

/// Build target
pub enum BuildTarget {
  Wasm,
  Native,
}

impl Default for BuildConfig {
  fn default() -> Self {
    Self {
      target: BuildTarget::Wasm,
      optimize: false,
      debug: true,
    }
  }
}

/// Build system for Lattice packages
#[allow(dead_code)]
pub struct BuildSystem {
  config: BuildConfig,
}

impl BuildSystem {
  /// Create a new build system
  pub fn new(config: BuildConfig) -> Self {
    Self { config }
  }

  /// Build a package
  pub fn build(&self, _package: &Package) -> crate::Result<()> {
    // TODO: Implement package building
    // TODO: Use self.config for build settings
    todo!("Package building not yet implemented")
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_build_system_creation() {
    let config = BuildConfig::default();
    let build_system = BuildSystem::new(config);
    assert!(matches!(build_system.config.target, BuildTarget::Wasm));
  }
}
