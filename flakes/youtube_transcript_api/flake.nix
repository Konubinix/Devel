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
          pname = "youtube_transcript_api";
          version = "0.6.2";
          format = "setuptools";
          src = pkgs.fetchFromGitHub {
            owner = "jdepoix";
            repo = "youtube-transcript-api";
            rev = "refs/tags/v${version}";
            leaveDotGit = true;
            sha256 = "sha256-9/sGWFyybcKgoits3uYebkdeDlzKGwq9GsVd2BfJPtI=";
          };

          propagatedBuildInputs = with pkgs.python3.pkgs; [
            setuptools
            requests
          ];

          # pythonImportsCheck = [ pname ];
          doCheck = false;

          meta = with pkgs.lib; {
            mainProgram = "youtube-transcript-api";
            description = "Transcript/Subtitle API";
            homepage = "https://github.com/jdepoix/youtube-transcript-api";
          };
        };
      });
}
