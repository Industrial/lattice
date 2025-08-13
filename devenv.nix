{pkgs, ...}: {
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
        description = "Check if project builds successfully";
        entry = "cargo build";
        pass_filenames = false;
      };

      # Test
      test = {
        enable = true;
        name = "cargo-test";
        description = "Run cargo tests";
        entry = "cargo test --workspace";
        pass_filenames = false;
      };

      # Coverage check
      coverage = {
        enable = true;
        name = "cargo-coverage-check";
        description = "Check test coverage (80% threshold)";
        entry = "cargo llvm-cov --fail-under-lines 80 --fail-under-functions 80";
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
}
