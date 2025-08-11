use clap::{Parser, Subcommand};
use lattice_compiler::{Compiler, CompilerConfig, CompilerTarget};

#[derive(Parser)]
#[clap(name = "lattice-compiler")]
#[clap(about = "Compiler for the Lattice functional programming language")]
struct Cli {
  #[clap(subcommand)]
  command: Commands,
}

#[derive(Subcommand)]
enum Commands {
  /// Build a .lat file to a .wasm module
  Build {
    #[clap(long, short)]
    input: String,
    #[clap(long, short, default_value = "out.wasm")]
    output: String,
    #[clap(long)]
    optimize: bool,
    #[clap(long)]
    debug: bool,
  },

  /// Check syntax without generating code
  Check {
    #[clap(long, short)]
    input: String,
  },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let cli = Cli::parse();

  match cli.command {
    Commands::Build {
      input,
      output,
      optimize,
      debug,
    } => {
      let config = CompilerConfig {
        optimize,
        debug_info: debug,
        target: CompilerTarget::Wasm,
      };

      let compiler = Compiler::new(config);
      let source = std::fs::read_to_string(&input)?;

      match compiler.compile(&source) {
        Ok(wasm_bytes) => {
          std::fs::write(&output, wasm_bytes)?;
          println!("Successfully compiled {} to {}", input, output);
          Ok(())
        }
        Err(e) => {
          eprintln!("Compilation failed: {}", e);
          std::process::exit(1);
        }
      }
    }

    Commands::Check { input } => {
      let _source = std::fs::read_to_string(&input)?;
      // TODO: Implement syntax checking
      println!("Syntax check for {} (not yet implemented)", input);
      Ok(())
    }
  }
}
