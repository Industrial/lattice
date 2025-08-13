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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_cli_build_command() {
    let args = vec!["latticec", "build", "-i", "input.lat", "-o", "output.wasm"];
    let cli = Cli::parse_from(args);

    match cli.command {
      Some(Commands::Build { input, output }) => {
        assert_eq!(input, "input.lat");
        assert_eq!(output, "output.wasm");
      }
      _ => panic!("Expected Build command"),
    }
  }

  #[test]
  fn test_cli_check_command() {
    let args = vec!["latticec", "check", "-i", "input.lat"];
    let cli = Cli::parse_from(args);

    match cli.command {
      Some(Commands::Check { input }) => {
        assert_eq!(input, "input.lat");
      }
      _ => panic!("Expected Check command"),
    }
  }

  #[test]
  fn test_cli_no_command() {
    let args = vec!["latticec"];
    let cli = Cli::parse_from(args);

    assert!(cli.command.is_none());
  }

  #[test]
  fn test_cli_help() {
    let args = vec!["latticec", "--help"];
    // This should not panic
    let _cli = Cli::parse_from(args);
  }

  #[test]
  fn test_cli_version() {
    let args = vec!["latticec", "--version"];
    // This should not panic
    let _cli = Cli::parse_from(args);
  }

  #[test]
  fn test_build_command_display() {
    let build_cmd = Commands::Build {
      input: "test.lat".to_string(),
      output: "test.wasm".to_string(),
    };

    // Test that the command can be created without panicking
    assert!(
      matches!(build_cmd, Commands::Build { input, output } if input == "test.lat" && output == "test.wasm")
    );
  }

  #[test]
  fn test_check_command_display() {
    let check_cmd = Commands::Check {
      input: "test.lat".to_string(),
    };

    // Test that the command can be created without panicking
    assert!(matches!(check_cmd, Commands::Check { input } if input == "test.lat"));
  }

  #[test]
  fn test_cli_struct_creation() {
    let cli = Cli { command: None };

    assert!(cli.command.is_none());
  }

  #[test]
  fn test_cli_with_build_command() {
    let build_cmd = Commands::Build {
      input: "input.lat".to_string(),
      output: "output.wasm".to_string(),
    };

    let cli = Cli {
      command: Some(build_cmd),
    };

    match cli.command {
      Some(Commands::Build { input, output }) => {
        assert_eq!(input, "input.lat");
        assert_eq!(output, "output.wasm");
      }
      _ => panic!("Expected Build command"),
    }
  }

  #[test]
  fn test_cli_with_check_command() {
    let check_cmd = Commands::Check {
      input: "input.lat".to_string(),
    };

    let cli = Cli {
      command: Some(check_cmd),
    };

    match cli.command {
      Some(Commands::Check { input }) => {
        assert_eq!(input, "input.lat");
      }
      _ => panic!("Expected Check command"),
    }
  }
}
