{
  description = "golang env";

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        deps = [ pkgs.go_1_22 pkgs.gopls pkgs.gotools pkgs.delve ];
      in {
        packages.default = pkgs.buildEnv {
          name = "go";
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
          description =
            "nix flake new -t github:konubinix/Devel/flakes/golang .";
        };
      };
}
