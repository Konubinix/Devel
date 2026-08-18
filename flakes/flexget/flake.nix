{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        flexgetWithDeps = pkgs.flexget.overridePythonAttrs (old: {
          propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ [
            pkgs.python3.pkgs.requests
            pkgs.python3.pkgs.beautifulsoup4
            pkgs.python3.pkgs.lxml
            pkgs.python3.pkgs.html2text
            pkgs.python3.pkgs.python-dateutil
            pkgs.python3.pkgs.six
          ];
        });
      in
      {
        packages.default = flexgetWithDeps;
      }
    );
}
