{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.default = pkgs.python3.pkgs.buildPythonApplication rec {
          pname = "colout";
          version = "1.1";
          format = "setuptools";

          src = pkgs.fetchFromGitHub {
            owner = "nojhan";
            repo = pname;
            rev = "refs/tags/v1.1";
            leaveDotGit = true;
            sha256 = "sha256-18UYwlVB8Dupy6KweuL/kmNPy58iMbnYXRsjf6Wc+OU=";
          };

          SETUPTOOLS_SCM_PRETEND_VERSION = version;

          propagatedBuildInputs = with pkgs.python3.pkgs; [
            setuptools
            setuptools_scm
            babel
            pygments
          ];

          # pythonImportsCheck = [ pname ];
          doCheck = false;

          meta = with pkgs.lib; {
            mainProgram = "colout";
            description = "Color Up Arbitrary Command Output";
            homepage = "https://pypi.org/project/colout/";
          };
        };
      });
}
