//! WebAssembly code generation support for the IR.
//!
//! This module provides utilities for preparing the IR for WASM code generation:
//! - Type mapping from Lattice types to WASM types
//! - Lowering passes for WASM-specific transformations
//! - ADT representation strategies
//! - Function import/export handling

use crate::ir::nodes::*;
use crate::ir::optimizations::OptimizationPass;
use crate::types::types::{PrimitiveType, Type};
use std::collections::HashMap;

// =============================================================================
// WASM Type System
// =============================================================================

/// WASM value types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WasmType {
  /// 32-bit integer
  I32,
  /// 64-bit integer
  I64,
  /// 32-bit float
  F32,
  /// 64-bit float
  F64,
  /// Reference to a function
  FuncRef,
  /// Reference to external data
  ExternRef,
}

impl std::fmt::Display for WasmType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      WasmType::I32 => write!(f, "i32"),
      WasmType::I64 => write!(f, "i64"),
      WasmType::F32 => write!(f, "f32"),
      WasmType::F64 => write!(f, "f64"),
      WasmType::FuncRef => write!(f, "funcref"),
      WasmType::ExternRef => write!(f, "externref"),
    }
  }
}

/// WASM function signature
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WasmFuncType {
  /// Parameter types
  pub params: Vec<WasmType>,
  /// Return types (WASM supports multiple returns)
  pub results: Vec<WasmType>,
}

impl WasmFuncType {
  /// Create a new function type
  pub fn new(params: Vec<WasmType>, results: Vec<WasmType>) -> Self {
    Self { params, results }
  }

  /// Create a function type with no parameters and a single result
  pub fn nullary(result: WasmType) -> Self {
    Self {
      params: vec![],
      results: vec![result],
    }
  }

  /// Create a function type with no return value
  pub fn void(params: Vec<WasmType>) -> Self {
    Self {
      params,
      results: vec![],
    }
  }
}

impl std::fmt::Display for WasmFuncType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let params = self
      .params
      .iter()
      .map(|p| p.to_string())
      .collect::<Vec<_>>()
      .join(" ");
    let results = self
      .results
      .iter()
      .map(|r| r.to_string())
      .collect::<Vec<_>>()
      .join(" ");

    if self.params.is_empty() && self.results.is_empty() {
      write!(f, "(func)")
    } else if self.params.is_empty() {
      write!(f, "(func (result {}))", results)
    } else if self.results.is_empty() {
      write!(f, "(func (param {}))", params)
    } else {
      write!(f, "(func (param {}) (result {}))", params, results)
    }
  }
}

// =============================================================================
// Type Mapping
// =============================================================================

/// Maps Lattice types to WASM types
#[derive(Debug, Default)]
pub struct TypeMapper {
  /// Cache of mapped types
  type_cache: HashMap<String, WasmType>,
  /// ADT layouts for tagged unions
  adt_layouts: HashMap<String, AdtLayout>,
}

impl TypeMapper {
  /// Create a new type mapper
  pub fn new() -> Self {
    Self::default()
  }

  /// Map a Lattice type to a WASM type
  pub fn map_type(&mut self, ty: &Type) -> WasmType {
    match ty {
      Type::Primitive(prim) => self.map_primitive(prim),
      Type::Variable(_) => {
        // Type variables are represented as i32 (boxed/tagged)
        WasmType::I32
      }
      Type::Function { .. } => {
        // Functions are represented as function references
        WasmType::FuncRef
      }
      Type::Product(_) | Type::Sum(_) => {
        // Compound types are represented as pointers (i32)
        WasmType::I32
      }
      Type::Application(_) => {
        // Type applications are resolved and then mapped
        WasmType::I32
      }
      Type::Reference(_)
      | Type::Constrained(_)
      | Type::Effect(_)
      | Type::Alias(_, _)
      | Type::Generic(_, _)
      | Type::Polymorphic(_) => {
        // Other types default to i32 (pointer representation)
        WasmType::I32
      }
    }
  }

  /// Map a primitive type to a WASM type
  pub fn map_primitive(&self, prim: &PrimitiveType) -> WasmType {
    match prim {
      PrimitiveType::Int => WasmType::I64,
      PrimitiveType::Float => WasmType::F64,
      PrimitiveType::Bool => WasmType::I32, // Booleans are i32 (0 or 1)
      PrimitiveType::Char => WasmType::I32, // Characters are Unicode code points
      PrimitiveType::String => WasmType::I32, // Strings are pointers
      PrimitiveType::Unit => WasmType::I32,   // Unit is represented as 0
    }
  }

  /// Map a Lattice function type to a WASM function type
  pub fn map_function_type(&mut self, params: &[Type], return_type: &Type) -> WasmFuncType {
    let wasm_params: Vec<WasmType> = params.iter().map(|p| self.map_type(p)).collect();
    let wasm_result = self.map_type(return_type);
    WasmFuncType::new(wasm_params, vec![wasm_result])
  }

  /// Register an ADT layout
  pub fn register_adt(&mut self, name: String, layout: AdtLayout) {
    self.adt_layouts.insert(name, layout);
  }

  /// Get the layout for an ADT
  pub fn get_adt_layout(&self, name: &str) -> Option<&AdtLayout> {
    self.adt_layouts.get(name)
  }
}

// =============================================================================
// ADT Representation
// =============================================================================

/// Layout strategy for algebraic data types in WASM
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AdtLayout {
  /// Simple enum (no payload) - represented as i32 tag
  SimpleEnum { variants: Vec<String> },
  /// Tagged union (variants with payloads)
  TaggedUnion {
    variants: Vec<VariantLayout>,
    /// Total size in bytes (aligned)
    size: u32,
  },
  /// Single-variant struct (no tag needed)
  Struct { fields: Vec<FieldLayout> },
}

/// Layout for a variant in a tagged union
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantLayout {
  /// Variant name
  pub name: String,
  /// Tag value
  pub tag: u32,
  /// Fields in this variant
  pub fields: Vec<FieldLayout>,
  /// Size of payload in bytes
  pub payload_size: u32,
}

/// Layout for a field
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldLayout {
  /// Field name (if named)
  pub name: Option<String>,
  /// WASM type of the field
  pub wasm_type: WasmType,
  /// Offset from start of struct/payload
  pub offset: u32,
  /// Size in bytes
  pub size: u32,
}

impl AdtLayout {
  /// Create a simple enum layout
  pub fn simple_enum(variants: Vec<String>) -> Self {
    Self::SimpleEnum { variants }
  }

  /// Create a tagged union layout
  pub fn tagged_union(variants: Vec<VariantLayout>, size: u32) -> Self {
    Self::TaggedUnion { variants, size }
  }

  /// Create a struct layout
  pub fn struct_layout(fields: Vec<FieldLayout>) -> Self {
    Self::Struct { fields }
  }

  /// Get the tag for a variant (if applicable)
  pub fn get_variant_tag(&self, name: &str) -> Option<u32> {
    match self {
      AdtLayout::SimpleEnum { variants } => {
        variants.iter().position(|v| v == name).map(|i| i as u32)
      }
      AdtLayout::TaggedUnion { variants, .. } => {
        variants.iter().find(|v| v.name == name).map(|v| v.tag)
      }
      AdtLayout::Struct { .. } => None,
    }
  }

  /// Calculate the size in bytes
  pub fn size(&self) -> u32 {
    match self {
      AdtLayout::SimpleEnum { .. } => 4, // i32 tag
      AdtLayout::TaggedUnion { size, .. } => *size,
      AdtLayout::Struct { fields } => fields.iter().map(|f| f.size).sum(),
    }
  }
}

/// Helper to compute field layouts with alignment
pub fn compute_field_layouts(fields: Vec<(Option<String>, WasmType)>) -> Vec<FieldLayout> {
  let mut layouts = Vec::new();
  let mut offset = 0u32;

  for (name, wasm_type) in fields {
    let size = wasm_type_size(wasm_type);
    let alignment = wasm_type_alignment(wasm_type);

    // Align offset
    offset = (offset + alignment - 1) & !(alignment - 1);

    layouts.push(FieldLayout {
      name,
      wasm_type,
      offset,
      size,
    });

    offset += size;
  }

  layouts
}

/// Get the size in bytes for a WASM type
pub fn wasm_type_size(ty: WasmType) -> u32 {
  match ty {
    WasmType::I32 | WasmType::F32 => 4,
    WasmType::I64 | WasmType::F64 => 8,
    WasmType::FuncRef | WasmType::ExternRef => 4, // Pointer size
  }
}

/// Get the alignment in bytes for a WASM type
pub fn wasm_type_alignment(ty: WasmType) -> u32 {
  wasm_type_size(ty) // Natural alignment
}

// =============================================================================
// Function Import/Export
// =============================================================================

/// Represents a WASM function import
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WasmImport {
  /// Module name
  pub module: String,
  /// Function name in the module
  pub name: String,
  /// Local alias (name used in Lattice code)
  pub alias: String,
  /// Function type
  pub func_type: WasmFuncType,
}

impl WasmImport {
  /// Create a new import
  pub fn new(module: String, name: String, alias: String, func_type: WasmFuncType) -> Self {
    Self {
      module,
      name,
      alias,
      func_type,
    }
  }
}

/// Represents a WASM function export
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WasmExport {
  /// Export name (visible to external code)
  pub name: String,
  /// Internal function name
  pub internal_name: String,
}

impl WasmExport {
  /// Create a new export
  pub fn new(name: String, internal_name: String) -> Self {
    Self {
      name,
      internal_name,
    }
  }

  /// Create an export where the external name matches the internal name
  pub fn same_name(name: String) -> Self {
    Self {
      name: name.clone(),
      internal_name: name,
    }
  }
}

/// Tracks imports and exports for a WASM module
#[derive(Debug, Default)]
pub struct ImportExportRegistry {
  imports: Vec<WasmImport>,
  exports: Vec<WasmExport>,
}

impl ImportExportRegistry {
  /// Create a new registry
  pub fn new() -> Self {
    Self::default()
  }

  /// Add an import
  pub fn add_import(&mut self, import: WasmImport) {
    self.imports.push(import);
  }

  /// Add an export
  pub fn add_export(&mut self, export: WasmExport) {
    self.exports.push(export);
  }

  /// Get all imports
  pub fn imports(&self) -> &[WasmImport] {
    &self.imports
  }

  /// Get all exports
  pub fn exports(&self) -> &[WasmExport] {
    &self.exports
  }

  /// Check if a function is imported
  pub fn is_imported(&self, name: &str) -> bool {
    self.imports.iter().any(|i| i.alias == name)
  }

  /// Get the import for a function
  pub fn get_import(&self, name: &str) -> Option<&WasmImport> {
    self.imports.iter().find(|i| i.alias == name)
  }
}

// =============================================================================
// WASM Lowering Pass
// =============================================================================

/// WASM lowering optimization pass.
///
/// Transforms high-level IR constructs into WASM-friendly forms:
/// - Resolves type annotations to WASM types
/// - Handles ADT construction and pattern matching
/// - Prepares function signatures for WASM
pub struct WasmLowering {
  made_changes: bool,
  type_mapper: TypeMapper,
  registry: ImportExportRegistry,
}

impl Default for WasmLowering {
  fn default() -> Self {
    Self::new()
  }
}

impl WasmLowering {
  /// Create a new WASM lowering pass
  pub fn new() -> Self {
    Self {
      made_changes: false,
      type_mapper: TypeMapper::new(),
      registry: ImportExportRegistry::new(),
    }
  }

  /// Get the type mapper
  pub fn type_mapper(&self) -> &TypeMapper {
    &self.type_mapper
  }

  /// Get a mutable reference to the type mapper
  pub fn type_mapper_mut(&mut self) -> &mut TypeMapper {
    &mut self.type_mapper
  }

  /// Get the import/export registry
  pub fn registry(&self) -> &ImportExportRegistry {
    &self.registry
  }

  /// Get a mutable reference to the registry
  pub fn registry_mut(&mut self) -> &mut ImportExportRegistry {
    &mut self.registry
  }

  /// Collect exported functions from the IR
  fn collect_exports(&mut self, ir: &IRNode) {
    for function in &ir.functions {
      if function.exported {
        self.registry.add_export(WasmExport::same_name(function.name.clone()));
      }
    }
  }

  /// Process type definitions to create ADT layouts
  fn process_type_definitions(&mut self, ir: &IRNode) {
    for type_def in &ir.type_definitions {
      let layout = self.create_adt_layout(type_def);
      self.type_mapper.register_adt(type_def.name.clone(), layout);
    }
  }

  /// Create an ADT layout from a type definition
  fn create_adt_layout(&self, type_def: &IRTypeDef) -> AdtLayout {
    // Check if it's a simple enum (no payloads)
    let all_empty = type_def.variants.iter().all(|v| v.fields.is_empty());

    if all_empty {
      return AdtLayout::simple_enum(
        type_def.variants.iter().map(|v| v.name.clone()).collect(),
      );
    }

    // Check if it's a single-variant struct
    if type_def.variants.len() == 1 {
      let variant = &type_def.variants[0];
      let fields: Vec<_> = variant
        .fields
        .iter()
        .enumerate()
        .map(|(i, ty)| {
          let wasm_ty = self.type_mapper.map_primitive(&PrimitiveType::Int); // Simplified
          (Some(format!("field_{}", i)), wasm_ty)
        })
        .collect();

      return AdtLayout::struct_layout(compute_field_layouts(fields));
    }

    // Otherwise, it's a tagged union
    let mut variants = Vec::new();
    let mut max_payload_size = 0u32;

    for (tag, variant) in type_def.variants.iter().enumerate() {
      let fields: Vec<_> = variant
        .fields
        .iter()
        .enumerate()
        .map(|(i, _ty)| {
          let wasm_ty = WasmType::I32; // Simplified - would use type_mapper
          (Some(format!("field_{}", i)), wasm_ty)
        })
        .collect();

      let field_layouts = compute_field_layouts(fields);
      let payload_size: u32 = field_layouts.iter().map(|f| f.offset + f.size).max().unwrap_or(0);

      max_payload_size = max_payload_size.max(payload_size);

      variants.push(VariantLayout {
        name: variant.name.clone(),
        tag: tag as u32,
        fields: field_layouts,
        payload_size,
      });
    }

    // Total size = 4 bytes (tag) + max payload size, aligned to 4 bytes
    let size = ((4 + max_payload_size) + 3) & !3;

    AdtLayout::tagged_union(variants, size)
  }
}

impl OptimizationPass for WasmLowering {
  fn name(&self) -> &'static str {
    "wasm_lowering"
  }

  fn run(&mut self, ir: IRNode) -> IRNode {
    self.made_changes = false;

    // Process type definitions
    self.process_type_definitions(&ir);

    // Collect exports
    self.collect_exports(&ir);

    // Note: Full lowering would transform the IR further:
    // - Replace high-level pattern matching with low-level tag checks
    // - Insert memory management calls
    // - Flatten nested expressions

    ir
  }

  fn made_changes(&self) -> bool {
    self.made_changes
  }
}

// =============================================================================
// WASM Module Builder
// =============================================================================

/// Builds a WASM module representation from lowered IR
#[derive(Debug)]
pub struct WasmModuleBuilder {
  /// Module name
  pub name: String,
  /// Function types (deduplicated)
  pub func_types: Vec<WasmFuncType>,
  /// Imports
  pub imports: Vec<WasmImport>,
  /// Exports
  pub exports: Vec<WasmExport>,
  /// Memory pages (initial, maximum)
  pub memory: Option<(u32, Option<u32>)>,
  /// Global variables
  pub globals: Vec<WasmGlobal>,
}

/// A WASM global variable
#[derive(Debug, Clone)]
pub struct WasmGlobal {
  pub name: String,
  pub ty: WasmType,
  pub mutable: bool,
  pub init_value: i64,
}

impl WasmModuleBuilder {
  /// Create a new module builder
  pub fn new(name: String) -> Self {
    Self {
      name,
      func_types: Vec::new(),
      imports: Vec::new(),
      exports: Vec::new(),
      memory: None,
      globals: Vec::new(),
    }
  }

  /// Set the memory configuration (pages)
  pub fn with_memory(mut self, initial: u32, maximum: Option<u32>) -> Self {
    self.memory = Some((initial, maximum));
    self
  }

  /// Add a function type (returns index)
  pub fn add_func_type(&mut self, func_type: WasmFuncType) -> u32 {
    // Check for existing type
    for (i, existing) in self.func_types.iter().enumerate() {
      if *existing == func_type {
        return i as u32;
      }
    }
    // Add new type
    let index = self.func_types.len() as u32;
    self.func_types.push(func_type);
    index
  }

  /// Add an import
  pub fn add_import(&mut self, import: WasmImport) {
    self.imports.push(import);
  }

  /// Add an export
  pub fn add_export(&mut self, export: WasmExport) {
    self.exports.push(export);
  }

  /// Add a global variable
  pub fn add_global(&mut self, global: WasmGlobal) {
    self.globals.push(global);
  }

  /// Build from an IR node
  pub fn from_ir(name: String, ir: &IRNode, lowering: &WasmLowering) -> Self {
    let mut builder = Self::new(name);

    // Add default memory (1 page = 64KB)
    builder = builder.with_memory(1, Some(16));

    // Add imports and exports from the lowering pass
    for import in lowering.registry().imports() {
      builder.add_import(import.clone());
    }
    for export in lowering.registry().exports() {
      builder.add_export(export.clone());
    }

    // Add function types for each function
    for function in &ir.functions {
      let params: Vec<WasmType> = function
        .parameters
        .iter()
        .map(|_| WasmType::I32) // Simplified - would use type_mapper
        .collect();

      let results = if function.return_type.is_some() {
        vec![WasmType::I32] // Simplified
      } else {
        vec![]
      };

      builder.add_func_type(WasmFuncType::new(params, results));
    }

    builder
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::lexer::SourceLocation;

  fn test_span() -> IRSpan {
    IRSpan::new(SourceLocation::start(), SourceLocation::new(1, 5, 4))
  }

  // =========================================================================
  // Type Mapping Tests
  // =========================================================================

  #[test]
  fn test_map_primitive_types() {
    let mapper = TypeMapper::new();

    assert_eq!(mapper.map_primitive(&PrimitiveType::Int), WasmType::I64);
    assert_eq!(mapper.map_primitive(&PrimitiveType::Float), WasmType::F64);
    assert_eq!(mapper.map_primitive(&PrimitiveType::Bool), WasmType::I32);
    assert_eq!(mapper.map_primitive(&PrimitiveType::Char), WasmType::I32);
    assert_eq!(mapper.map_primitive(&PrimitiveType::String), WasmType::I32);
    assert_eq!(mapper.map_primitive(&PrimitiveType::Unit), WasmType::I32);
  }

  #[test]
  fn test_map_type() {
    let mut mapper = TypeMapper::new();

    // Primitive types
    assert_eq!(
      mapper.map_type(&Type::primitive(PrimitiveType::Int)),
      WasmType::I64
    );

    // Type variables default to i32
    assert_eq!(mapper.map_type(&Type::var(0)), WasmType::I32);

    // Function types map to funcref
    assert_eq!(
      mapper.map_type(&Type::function(vec![], Type::primitive(PrimitiveType::Int))),
      WasmType::FuncRef
    );
  }

  #[test]
  fn test_map_function_type() {
    let mut mapper = TypeMapper::new();

    let func_type = mapper.map_function_type(
      &[
        Type::primitive(PrimitiveType::Int),
        Type::primitive(PrimitiveType::Bool),
      ],
      &Type::primitive(PrimitiveType::Float),
    );

    assert_eq!(func_type.params, vec![WasmType::I64, WasmType::I32]);
    assert_eq!(func_type.results, vec![WasmType::F64]);
  }

  // =========================================================================
  // ADT Layout Tests
  // =========================================================================

  #[test]
  fn test_simple_enum_layout() {
    let layout = AdtLayout::simple_enum(vec!["Red".into(), "Green".into(), "Blue".into()]);

    assert_eq!(layout.get_variant_tag("Red"), Some(0));
    assert_eq!(layout.get_variant_tag("Green"), Some(1));
    assert_eq!(layout.get_variant_tag("Blue"), Some(2));
    assert_eq!(layout.get_variant_tag("Unknown"), None);
    assert_eq!(layout.size(), 4); // i32 tag
  }

  #[test]
  fn test_struct_layout() {
    let fields = compute_field_layouts(vec![
      (Some("x".into()), WasmType::I32),
      (Some("y".into()), WasmType::I64),
      (Some("z".into()), WasmType::I32),
    ]);

    assert_eq!(fields.len(), 3);
    assert_eq!(fields[0].offset, 0);
    assert_eq!(fields[0].size, 4);
    assert_eq!(fields[1].offset, 8); // Aligned to 8 for i64
    assert_eq!(fields[1].size, 8);
    assert_eq!(fields[2].offset, 16);
    assert_eq!(fields[2].size, 4);
  }

  #[test]
  fn test_tagged_union_layout() {
    let variants = vec![
      VariantLayout {
        name: "None".into(),
        tag: 0,
        fields: vec![],
        payload_size: 0,
      },
      VariantLayout {
        name: "Some".into(),
        tag: 1,
        fields: vec![FieldLayout {
          name: Some("value".into()),
          wasm_type: WasmType::I32,
          offset: 0,
          size: 4,
        }],
        payload_size: 4,
      },
    ];

    let layout = AdtLayout::tagged_union(variants, 8);

    assert_eq!(layout.get_variant_tag("None"), Some(0));
    assert_eq!(layout.get_variant_tag("Some"), Some(1));
    assert_eq!(layout.size(), 8);
  }

  // =========================================================================
  // Import/Export Tests
  // =========================================================================

  #[test]
  fn test_import_export_registry() {
    let mut registry = ImportExportRegistry::new();

    registry.add_import(WasmImport::new(
      "console".into(),
      "log".into(),
      "print".into(),
      WasmFuncType::void(vec![WasmType::I32]),
    ));

    registry.add_export(WasmExport::same_name("main".into()));

    assert!(registry.is_imported("print"));
    assert!(!registry.is_imported("unknown"));
    assert_eq!(registry.imports().len(), 1);
    assert_eq!(registry.exports().len(), 1);
  }

  // =========================================================================
  // WASM Function Type Tests
  // =========================================================================

  #[test]
  fn test_wasm_func_type_display() {
    let nullary = WasmFuncType::nullary(WasmType::I32);
    assert_eq!(nullary.to_string(), "(func (result i32))");

    let void = WasmFuncType::void(vec![WasmType::I32, WasmType::I64]);
    assert_eq!(void.to_string(), "(func (param i32 i64))");

    let full = WasmFuncType::new(vec![WasmType::I32], vec![WasmType::I64]);
    assert_eq!(full.to_string(), "(func (param i32) (result i64))");
  }

  // =========================================================================
  // WASM Lowering Tests
  // =========================================================================

  #[test]
  fn test_wasm_lowering_pass() {
    let span = test_span();
    let mut pass = WasmLowering::new();

    // Create a simple IR with an exported function
    let function = IRFunction::new(
      "main".to_string(),
      vec![],
      None,
      vec![IRStatement::Return {
        expression: Some(IRAtom::Literal(IRLiteral::Integer { value: 42, span })),
        span,
      }],
      span,
    )
    .exported();

    let ir = IRNode::new(vec![function], vec![], vec![], span);
    let _optimized = pass.run(ir);

    // Check that export was collected
    assert_eq!(pass.registry().exports().len(), 1);
    assert_eq!(pass.registry().exports()[0].name, "main");
  }

  #[test]
  fn test_wasm_lowering_type_definitions() {
    let span = test_span();
    let mut pass = WasmLowering::new();

    // Create a simple enum type
    let type_def = IRTypeDef::new(
      "Color".to_string(),
      vec![],
      vec![
        IRTypeVariant::new("Red".to_string(), vec![], span),
        IRTypeVariant::new("Green".to_string(), vec![], span),
        IRTypeVariant::new("Blue".to_string(), vec![], span),
      ],
      span,
    );

    let ir = IRNode::new(vec![], vec![type_def], vec![], span);
    let _optimized = pass.run(ir);

    // Check that ADT layout was created
    let layout = pass.type_mapper().get_adt_layout("Color");
    assert!(layout.is_some());
    assert!(matches!(layout.unwrap(), AdtLayout::SimpleEnum { .. }));
  }

  // =========================================================================
  // Module Builder Tests
  // =========================================================================

  #[test]
  fn test_wasm_module_builder() {
    let mut builder = WasmModuleBuilder::new("test".to_string())
      .with_memory(1, Some(16));

    // Add function types
    let type_idx = builder.add_func_type(WasmFuncType::nullary(WasmType::I32));
    assert_eq!(type_idx, 0);

    // Adding same type should return same index
    let type_idx2 = builder.add_func_type(WasmFuncType::nullary(WasmType::I32));
    assert_eq!(type_idx2, 0);

    // Different type should get new index
    let type_idx3 = builder.add_func_type(WasmFuncType::nullary(WasmType::I64));
    assert_eq!(type_idx3, 1);

    assert_eq!(builder.func_types.len(), 2);
    assert_eq!(builder.memory, Some((1, Some(16))));
  }

  #[test]
  fn test_wasm_type_size_and_alignment() {
    assert_eq!(wasm_type_size(WasmType::I32), 4);
    assert_eq!(wasm_type_size(WasmType::I64), 8);
    assert_eq!(wasm_type_size(WasmType::F32), 4);
    assert_eq!(wasm_type_size(WasmType::F64), 8);

    assert_eq!(wasm_type_alignment(WasmType::I32), 4);
    assert_eq!(wasm_type_alignment(WasmType::I64), 8);
  }
}

