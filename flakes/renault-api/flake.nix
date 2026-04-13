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
        packages.default = pkgs.python3.pkgs.toPythonApplication
          (pkgs.python3.pkgs.renault-api.overridePythonAttrs (old: {
            dependencies = (old.dependencies or [ ])
              ++ old.optional-dependencies.cli;
          }));
      });
}
