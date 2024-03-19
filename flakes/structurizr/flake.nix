{
  description = "Example Gradle project";
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        src = pkgs.fetchurl {
          url =
            "https://github.com/structurizr/cli/releases/download/v2024.03.03/structurizr-cli.zip";
          sha256 = "sha256-4M/7iLW5mLu+rkKKRjzwARVOgmaM6MYbn4fMsnQ7sfc=";
        };
      in {
        packages.src = src;
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "structurizr";
          version = "0.1.0";

          src = src;

          buildInputs = [ pkgs.unzip ];

          installPhase = ''
            runHook preInstall

            mkdir -p $out
            pushd $out
            unzip ${src}
            mkdir bin
            ln -sr structurizr.sh bin/structurizr
            popd

            runHook postInstall
          '';
        };
      });
}
