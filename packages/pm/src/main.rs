use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "lattice")]
#[command(about = "Lattice language package manager")]
#[command(version = "0.1.0")]
struct Cli {
  #[command(subcommand)]
  command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
  /// Initialize a new Lattice package
  Init {
    /// Package name
    name: String,
  },
  /// Install dependencies
  Install,
  /// Build a package
  Build,
}

fn main() {
  let cli = Cli::parse();

  match &cli.command {
    Some(Commands::Init { name }) => {
      println!(
        "Initializing new Lattice package: {} (not yet implemented)",
        name
      );
    }
    Some(Commands::Install) => {
      println!("Installing dependencies (not yet implemented)");
    }
    Some(Commands::Build) => {
      println!("Building package (not yet implemented)");
    }
    None => {
      println!("Lattice package manager - use --help for usage information");
    }
  }
}
