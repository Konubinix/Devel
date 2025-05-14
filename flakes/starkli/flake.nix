{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.default = pkgs.stdenv.mkDerivation rec {
          name = "starkli";
          version = "0.4.1";

          dontUnpack = true;
          dontPatch = true;
          dontConfigure = true;
          dontBuild = true;
          doCheck = false;
          buildInputs = [ ];

          installPhase = ''
            runHook preInstall

            tar xf ${src}

            mkdir -p $out/bin
            ls
            mv starkli $out/bin/

            runHook postInstall
          '';
          src = pkgs.fetchurl {
            url =
              "https://github.com/xJonathanLEI/starkli/releases/download/v${version}/starkli-x86_64-unknown-linux-gnu.tar.gz";
            sha256 = "sha256-QhfM11wRYn5RPhxuQ5W03/3r4rinSUvJiek8K4hbi+M=";
          };
        };
      });
}
