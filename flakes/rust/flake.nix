{
  description = "Rust env";

  inputs = {
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, fenix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        deps =
          (with fenix.packages.${system}.stable; # stable may be beta, (for nigthly: minimal, default, complete), latest
            [ (withComponents [ "cargo" "rustc" "rust-src" "rust-analyzer" ]) ])
          ++ (
            # for openssl-sys
            with pkgs; [ pkg-config openssl ]) ++ ( # for backtrace
              with pkgs; [ libunwind ])
          ++ (with pkgs; [ llvm llvmPackages.libclang ]);
      in rec {
        packages.default = pkgs.buildEnv {
          name = "rust";
          paths = with pkgs; deps;
        };
        devShell = pkgs.mkShell {
          buildInputs = deps;
          shellHook = ''
            export LIBCLANG_PATH="${pkgs.llvmPackages.libclang.lib}/lib";
          '';
        };
      }) // {
        templates.default = {
          path = ./templates/flake;
          description = "nix flake new -t github:konubinix/Devel/flakes/rust .";
        };
      };
}
