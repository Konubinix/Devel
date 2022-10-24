{
  description = "golang env";

  inputs = {
    konubinix-golang.url = "/home/sam/Prog/devel/flakes/golang";
    nixpkgs.url = "github:NixOS/nixpkgs/22.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, konubinix-golang, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = [ konubinix-golang.devShell.${system}.buildInputs ];
          shellHook = konubinix-golang.devShell.${system}.shellHook;
        };
      });
}
