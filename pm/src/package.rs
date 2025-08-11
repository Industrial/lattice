//! Package management for Lattice

use crate::dependency::DependencySpec;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Package manifest structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageManifest {
  pub name: String,
  pub version: String,
  pub description: Option<String>,
  pub entry: String,
  pub dependencies: std::collections::HashMap<String, DependencySpec>,
  pub dev_dependencies: std::collections::HashMap<String, DependencySpec>,
  pub lattice_version: String,
  pub targets: Vec<Target>,
}

/// Package target configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Target {
  pub name: String,
  pub kind: TargetKind,
  pub wasm_features: Vec<WasmFeature>,
}

/// Target kind
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TargetKind {
  Wasm,
  Native,
}

/// WASM features
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WasmFeature {
  Gc,
  Threads,
  ReferenceTypes,
  BulkMemory,
}

/// Package representation
pub struct Package {
  pub manifest: PackageManifest,
  pub path: PathBuf,
}

impl Package {
  /// Create a new package from a manifest and path
  pub fn new(manifest: PackageManifest, path: PathBuf) -> Self {
    Self { manifest, path }
  }

  /// Load a package from a directory
  pub fn load(path: PathBuf) -> crate::Result<Self> {
    let manifest_path = path.join("lattice.json");
    let manifest_content = std::fs::read_to_string(manifest_path)?;
    let manifest: PackageManifest = serde_json::from_str(&manifest_content)?;
    Ok(Self::new(manifest, path))
  }

  /// Save the package manifest
  pub fn save(&self) -> crate::Result<()> {
    let manifest_path = self.path.join("lattice.json");
    let manifest_content = serde_json::to_string_pretty(&self.manifest)?;
    std::fs::write(manifest_path, manifest_content)?;
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_package_manifest_serialization() {
    let manifest = PackageManifest {
      name: "test-package".to_string(),
      version: "0.1.0".to_string(),
      description: Some("A test package".to_string()),
      entry: "src/main.lat".to_string(),
      dependencies: std::collections::HashMap::new(),
      dev_dependencies: std::collections::HashMap::new(),
      lattice_version: "0.1.0".to_string(),
      targets: vec![],
    };

    let json = serde_json::to_string(&manifest).unwrap();
    let deserialized: PackageManifest = serde_json::from_str(&json).unwrap();
    assert_eq!(manifest.name, deserialized.name);
  }
}
