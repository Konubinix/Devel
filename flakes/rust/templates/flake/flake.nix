{
  description = "Rust env";

  inputs = {
    konubinix-rust.url = "/home/sam/Prog/devel/flakes/rust";
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, konubinix-rust, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = [ konubinix-rust.devShell.${system}.buildInputs ];
          shellHook = konubinix-rust.devShell.${system}.shellHook;
        };
      });
}
