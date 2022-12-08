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
          pname = "mir.msmtpq";
          version = "1.0.0";

          src = pkgs.python3.pkgs.fetchPypi {
            inherit pname version;
            hash = "sha256-iAq0oXs5pu+q02wJjTs/5U724RyRmHr/KOW157aYPzc=";
          };
          permitUserSite =
            true; # so that I call still call clk commands in the msmtp configuration
          propagatedBuildInputs = with pkgs.python3; [ ];

          doCheck = false; # attempts to access various URLs

          pythonImportsCheck = [ pname ];

          meta = with pkgs.lib; {
            mainProgram = "msmtpq";
            description = "Message queue for msmtp";
            homepage = "https://pypi.org/project/mir.msmtpq/";
          };
        };
      });
}
