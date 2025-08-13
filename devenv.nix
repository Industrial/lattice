{pkgs, ...}: let
  # Import Naersk for building Rust packages
  naersk = pkgs.callPackage (pkgs.fetchFromGitHub {
    owner = "nix-community";
    repo = "naersk";
    rev = "master";
    sha256 = "sha256-uldUBFkZe/E7qbvxa3mH1ItrWZyT6w1dBKJQF/3ZSsc=";
  }) {};
in {
  # Name of the project with version
  name = "lattice";

  # Languages
  languages = {
    rust = {
      enable = true;
      channel = "stable";
      components = [
        "cargo"
        "clippy"
        "rust-analyzer"
        "rustc"
        "rustfmt"
        "llvm-tools-preview"
      ];
      targets = [
        "wasm32-unknown-unknown"
      ];
    };
  };

  # Development packages
  packages = with pkgs; [
    # Rust tools
    cargo-nextest
    clippy
    rust-analyzer
    rustfmt
    cargo-llvm-cov

    # Development tools
    direnv
    pre-commit

    # Formatting tools
    alejandra

    # Added from the code block
    cargo-watch

    # TaskMaster AI dependencies
    bun
    nodejs_latest
  ];

  # Pre-commit hooks
  git-hooks = {
    hooks = {
      # Rust checks
      cargo-check = {
        enable = true;
      };
      clippy = {
        enable = true;
      };
      rustfmt = {
        enable = true;
      };

      # Build check
      build = {
        enable = true;
        name = "cargo-build";
        description = "Check that the project builds";
        entry = "cargo build";
        pass_filenames = false;
      };

      # Test coverage and cleanup (runs tests, checks coverage, then cleans up)
      test-coverage-cleanup = {
        enable = true;
        name = "cargo-test-coverage-cleanup";
        description = "Run tests with coverage (80% threshold) and clean up artifacts";
        entry = "bash -c 'cargo llvm-cov test --fail-under-lines 80 --fail-under-functions 80 && cargo llvm-cov clean'";
        pass_filenames = false;
      };
    };
  };

  # Automatic commands
  enterShell = ''
    echo "🦀 Running initial cargo build..."
    cargo build
  '';

  processes = {
    cargo-watch = {
      exec = "cargo watch -x check -x test";
    };
  };

  # Naersk build outputs - these are built on-demand, not during shell init
  outputs = {
    lattice = naersk.buildPackage {
      name = "lattice";
      version = "0.1.0";
      pname = "lattice";
      root = ./.;
    };
  };
}
