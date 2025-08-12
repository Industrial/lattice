use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "latticec")]
#[command(about = "Lattice language compiler")]
#[command(version = "0.1.0")]
struct Cli {
  #[command(subcommand)]
  command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
  /// Build a Lattice source file to WASM
  Build {
    /// Input Lattice source file
    #[arg(short, long)]
    input: String,
    /// Output WASM file
    #[arg(short, long)]
    output: String,
  },
  /// Check syntax without generating code
  Check {
    /// Input Lattice source file
    #[arg(short, long)]
    input: String,
  },
}

fn main() {
  let cli = Cli::parse();

  match &cli.command {
    Some(Commands::Build { input, output }) => {
      println!("Building {} to {} (not yet implemented)", input, output);
    }
    Some(Commands::Check { input }) => {
      println!("Checking syntax of {} (not yet implemented)", input);
    }
    None => {
      println!("Lattice compiler - use --help for usage information");
    }
  }
}
