# Lattice — repository skeleton

This document contains a complete initial git repository skeleton for **Lattice** — a small, pure, statically-typed Hindley–Milner functional language targeting WASM (server + browser). It follows your confirmed decisions and includes a minimal Rust-based compiler stub that emits a tiny `.wasm`, a prototype package manager CLI, a minimal stdlib prelude, CI, and project metadata.

> **Do not edit here** — this document is a single representation of the repository. Use it to copy files into a git repo (or I can generate the repo files if you'd like me to run processes). Below each file path the file contents are provided.

---

## Repository file tree

```
lattice/                     # repo root
├── Cargo.toml                # workspace manifest
├── README.md
├── LICENSE                   # Unlicense
├── .gitignore
├── Makefile
├── .github/
│   └── workflows/
│       └── ci.yml
├── compiler/                 # Rust compiler crate (lattice-compiler)
│   ├── Cargo.toml
│   └── src/
│       └── main.rs
├── pm/                       # package manager prototype (lattice-pm)
│   ├── Cargo.toml
│   └── src/
│       └── main.rs
├── stdlib/
│   └── prelude.lat
└── examples/                 # empty (you asked for no examples for now)
    └── README.md
```

---

## Root Cargo.toml (workspace)

```toml
[workspace]
members = [
  "compiler",
  "pm",
]

[profile.release]
opt-level = 3
lto = true
codegen-units = 1
```

---

## README.md

```md
# Lattice

Lattice is a small, pure, statically-typed functional language (Hindley–Milner inference, algebraic data types, algebraic effects with handlers) that targets WebAssembly in both server (Wasmtime) and browser environments.

This repository is an initial skeleton with:

- `compiler/` — a Rust-based compiler stub (`lattice-compiler`) which parses arguments and emits a minimal `.wasm` module. The compiler is intentionally a scaffold with clear TODOs where front-end and codegen will be implemented.
- `pm/` — a tiny package manager prototype (`lattice-pm`) that can initialize a package and write a `lattice.json` manifest.
- `stdlib/prelude.lat` — minimal prelude with basic type declarations.
- GitHub Actions CI that builds the Rust workspace.

## Getting started

Requirements: Rust (1.70+ recommended), cargo.

Build everything:

```sh
cargo build --release
```

Build the compiler and use it to produce a trivial wasm file:

```sh
# compile the compiler
cargo build -p lattice-compiler --release
# run the compiler (it will emit out.wasm)
./target/release/lattice-compiler build --input stdlib/prelude.lat --output out.wasm
```

Create a new Lattice package (prototype pm):

```sh
cargo run -p lattice-pm -- init my-package
# This creates ./my-package/lattice.json and scaffolds an entry file
```

## Goals & next steps

- Implement parser and typed AST (HM inference) in `compiler/src/`.
- Implement intermediate representation and codegen that emits idiomatic WASM, with host-GC when available and a fallback RC runtime.
- Expand `pm/` into a real package manager with registry integration.

---

## License

This repository is released under the **Unlicense**. See `LICENSE` file.
```

---

## LICENSE — Unlicense

```text
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
... (standard Unlicense text continues)

For full text, see https://unlicense.org
```

---

## .gitignore

```gitignore
/target
**/*.wasm
**/*.dbg
**/*.log
/node_modules
.DS_Store
```

---

## Makefile

```makefile
.PHONY: all build clean
all: build

build:
	cargo build --release

clean:
	cargo clean
```

---

## .github/workflows/ci.yml

```yaml
name: CI
on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install Rust
        uses: dtolnay/rust-toolchain@v1
        with:
          toolchain: stable
      - name: Build workspace
        run: cargo build --workspace --release
```

---

## compiler/Cargo.toml

```toml
[package]
name = "lattice-compiler"
version = "0.1.0"
edition = "2021"

[dependencies]
clap = "4.5"
anyhow = "1.0"
wasm-encoder = "0.236"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

---

## compiler/src/main.rs

```rust
use std::fs;
use std::path::PathBuf;
use clap::{Parser, Subcommand};
use anyhow::Result;
use wasm_encoder::{Module, Function, Instruction, CodeSection, FunctionSection, TypeSection};

#[derive(Parser)]
#[clap(name = "lattice-compiler")]
struct Cli {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Build a .lat file to a .wasm module
    Build {
        #[clap(long, short)]
        input: PathBuf,
        #[clap(long, short, default_value = "out.wasm")]
        output: PathBuf,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Build { input, output } => build(&input, &output)?,
    }
    Ok(())
}

fn build(input: &PathBuf, output: &PathBuf) -> Result<()> {
    // Read the input source (currently ignored by the stub)
    let _src = fs::read_to_string(input).unwrap_or_default();

    // Emit a minimal wasm module with one function `lattice_start` that returns i32 0.
    let mut module = Module::new();

    // Type section: (func (result i32))
    let mut types = TypeSection::new();
    let type_idx = types.function(vec![], vec![wasm_encoder::ValType::I32]);
    module.section(&types);

    // Function section: one function with the type above
    let mut funcs = FunctionSection::new();
    funcs.function(type_idx);
    module.section(&funcs);

    // Code section: body that returns i32.const 0
    let mut codes = CodeSection::new();
    let mut func = Function::new(vec![]);
    func.instruction(&Instruction::I32Const(0));
    func.instruction(&Instruction::End);
    codes.function(&func);
    module.section(&codes);

    // Export the function as "lattice_start"
    use wasm_encoder::ExportSection;
    let mut exports = ExportSection::new();
    exports.export("lattice_start", wasm_encoder::Export::Func(0));
    module.section(&exports);

    // Finish and write
    let wasm_bytes = module.finish();
    fs::write(output, wasm_bytes)?;

    println!("Wrote {}", output.display());
    Ok(())
}
```

---

## pm/Cargo.toml

```toml
[package]
name = "lattice-pm"
version = "0.1.0"
edition = "2021"

[dependencies]
clap = "4.5"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
anyhow = "1.0"
```

---

## pm/src/main.rs

```rust
use std::fs;
use std::path::PathBuf;
use clap::{Parser, Subcommand};
use serde::{Serialize, Deserialize};
use anyhow::Result;

#[derive(Parser)]
#[clap(name = "lattice-pm")]
struct Cli {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Initialize a new lattice package in the given directory
    Init {
        name: String,
    },
}

#[derive(Serialize, Deserialize)]
struct Manifest {
    name: String,
    version: String,
    entry: String,
    dependencies: serde_json::Value,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Commands::Init { name } => init_package(&name)?,
    }
    Ok(())
}

fn init_package(name: &str) -> Result<()> {
    let dir = PathBuf::from(name);
    fs::create_dir_all(&dir)?;

    let manifest = Manifest {
        name: name.to_string(),
        version: "0.1.0".to_string(),
        entry: "main.lat".to_string(),
        dependencies: serde_json::json!({}),
    };

    let manifest_str = serde_json::to_string_pretty(&manifest)?;
    fs::write(dir.join("lattice.json"), manifest_str)?;

    // Create a minimal entry file
    let entry = "-- Lattice package entry (main.lat)\nfn main : IO Int\nfn main = 0\n";
    fs::write(dir.join("main.lat"), entry)?;

    println!("Initialized package '{}'", name);
    Ok(())
}
```

---

## stdlib/prelude.lat

```text
-- Lattice prelude (very small)

-- Basic types
data Bool = True | False

data Option a = None | Some a

data Result e a = Err e | Ok a

-- Lists (linked)
data List a = Nil | Cons a (List a)

-- Type aliases
type Int = i64
type Float = f64

type String = [Char]

-- Effect placeholders (declaration style)
effect IO : * -> *
effect Async : * -> *
effect Host : * -> *

-- A trivial function example (type-only)
-- add : Int -> Int -> Int
-- add x y = x + y

-- TODO: populate with useful primitives and handlers
```

---

## examples/README.md

```md
No examples yet — intentionally empty as requested. Use `pm` to scaffold packages and compile them with `lattice-compiler`.
```

