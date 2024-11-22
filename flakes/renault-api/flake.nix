{
  description = "Flake for renault-api with optional dependencies";

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { system = system; };
      in {
        packages.default = pkgs.python3.withPackages (ps:
          with ps;
          [ renault-api ] ++ renault-api.optional-dependencies.cli);
      });
}
