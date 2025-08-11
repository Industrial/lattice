use clap::{Parser, Subcommand};
use lattice_pm::{PackageManager, PackageManagerConfig};

#[derive(Parser)]
#[clap(name = "lattice-pm")]
#[clap(about = "Package manager for the Lattice language")]
struct Cli {
  #[clap(subcommand)]
  command: Commands,
}

#[derive(Subcommand)]
enum Commands {
  /// Initialize a new Lattice package
  Init {
    name: String,
    #[clap(long)]
    description: Option<String>,
    #[clap(long, default_value = "0.1.0")]
    version: String,
  },

  /// Install package dependencies
  Install {
    #[clap(long)]
    package_dir: Option<String>,
  },

  /// Build a package
  Build {
    #[clap(long)]
    package_dir: Option<String>,
    #[clap(long)]
    optimize: bool,
  },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let cli = Cli::parse();

  let config = PackageManagerConfig::default();
  let pm = PackageManager::new(config);

  match cli.command {
    Commands::Init {
      name,
      description: _,
      version: _,
    } => {
      let path = std::path::PathBuf::from(&name);
      pm.init_package(&name, &path)?;
      println!("Initialized package '{}' in {}", name, path.display());
    }

    Commands::Install { package_dir } => {
      let dir = package_dir.unwrap_or_else(|| ".".to_string());
      let path = std::path::PathBuf::from(dir);
      let package = lattice_pm::Package::load(path)?;
      pm.install_dependencies(&package)?;
      println!(
        "Dependencies installed for package '{}'",
        package.manifest.name
      );
    }

    Commands::Build {
      package_dir,
      optimize: _,
    } => {
      let dir = package_dir.unwrap_or_else(|| ".".to_string());
      let path = std::path::PathBuf::from(dir);
      let package = lattice_pm::Package::load(path)?;
      // TODO: Implement building with optimize flag
      println!(
        "Building package '{}' (not yet implemented)",
        package.manifest.name
      );
    }
  }

  Ok(())
}
