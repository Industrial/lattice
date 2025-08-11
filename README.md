# Lattice

Lattice is a small, pure, statically-typed functional language (Hindley–Milner inference, algebraic data types, algebraic effects with handlers) that targets WebAssembly in both server (Wasmtime) and browser environments.

This repository contains a Rust-based implementation with the following components:

- `compiler/` — A Rust-based compiler (`lattice-compiler`) which will parse Lattice source code and emit WASM modules
- `pm/` — A package manager (`lattice-pm`) for managing Lattice packages and dependencies
- `stdlib/` — Standard library (`lattice-stdlib`) providing core types, functions, and effect handlers

## Project Status

This is an **early development version** with the basic project structure in place. The compiler is currently a scaffold with placeholder implementations for all major components.

## Getting Started

### Prerequisites

- Rust (1.70+ recommended)
- Cargo (comes with Rust)

### Building the Project

Build all components:

```sh
cargo build --release
```

Build individual components:

```sh
# Build compiler only
cargo build -p lattice-compiler --release

# Build package manager only
cargo build -p lattice-pm --release

# Build standard library only
cargo build -p lattice-stdlib --release
```

### Running Tests

Run all tests:

```sh
cargo test --workspace
```

Run tests for specific components:

```sh
# Test compiler
cargo test -p lattice-compiler

# Test package manager
cargo test -p lattice-pm

# Test standard library
cargo test -p lattice-stdlib
```

### Using the Compiler

The compiler is currently a placeholder. Once implemented, it will support:

```sh
# Compile a Lattice source file to WASM
cargo run -p lattice-compiler -- build --input src/main.lat --output main.wasm

# Check syntax without generating code
cargo run -p lattice-compiler -- check --input src/main.lat
```

### Using the Package Manager

The package manager is currently a placeholder. Once implemented, it will support:

```sh
# Initialize a new Lattice package
cargo run -p lattice-pm -- init my-package

# Install dependencies
cargo run -p lattice-pm -- install

# Build a package
cargo run -p lattice-pm -- build
```

## Project Structure

```
lattice/
├── Cargo.toml                 # Workspace manifest
├── README.md                  # This file
├── compiler/                  # Lattice compiler crate
│   ├── Cargo.toml
│   └── src/
│       ├── lib.rs            # Compiler library
│       ├── main.rs           # Compiler binary
│       ├── lexer.rs          # Lexical analysis
│       ├── parser.rs         # Syntax parsing
│       ├── type_checker.rs   # Type checking
│       ├── code_generator.rs # Code generation
│       └── error.rs          # Error handling
├── pm/                       # Package manager crate
│   ├── Cargo.toml
│   └── src/
│       ├── lib.rs            # Package manager library
│       ├── main.rs           # Package manager binary
│       ├── package.rs        # Package management
│       ├── dependency.rs     # Dependency resolution
│       ├── registry.rs       # Package registry
│       └── build.rs          # Build system
└── stdlib/                   # Standard library crate
    ├── Cargo.toml
    └── src/
        ├── lib.rs            # Standard library
        ├── types.rs          # Core types
        ├── functions.rs      # Core functions
        └── effects.rs        # Effect system
```

## Development

### Adding Dependencies

To add dependencies to a specific crate, edit the appropriate `Cargo.toml` file:

```toml
# Example: adding a dependency to the compiler
[dependencies]
new_dependency = "1.0"
```

### Running Specific Tests

```sh
# Run a specific test
cargo test -p lattice-compiler test_name

# Run tests with output
cargo test -p lattice-compiler -- --nocapture
```

### Documentation

Generate documentation:

```sh
# Generate docs for all crates
cargo doc --workspace --open

# Generate docs for specific crate
cargo doc -p lattice-compiler --open
```

## Goals & Next Steps

### Phase 1: Foundation (Current)
- ✅ Basic project structure and workspace setup
- ✅ Placeholder implementations for all major components
- ✅ Basic testing framework

### Phase 2: Core Language Implementation
- [ ] Implement lexer with proper tokenization
- [ ] Implement parser with AST construction
- [ ] Implement basic type system and type checking
- [ ] Implement WASM code generation

### Phase 3: Advanced Features
- [ ] Implement Hindley-Milner type inference
- [ ] Implement algebraic data types
- [ ] Implement algebraic effects with handlers
- [ ] Implement standard library functions

### Phase 4: Package Management
- [ ] Implement dependency resolution
- [ ] Implement package registry
- [ ] Implement build system
- [ ] Implement package installation

### Phase 5: Tooling & Ecosystem
- [ ] Language server implementation
- [ ] IDE support and extensions
- [ ] Performance optimization
- [ ] Comprehensive testing suite

## Contributing

This project is in early development. Contributions are welcome, but please note:

1. The codebase is currently a scaffold with placeholder implementations
2. Major architectural decisions are still being made
3. Focus on building the foundation before adding advanced features

## License

This repository is released under the **Unlicense**. See `LICENSE` file.

