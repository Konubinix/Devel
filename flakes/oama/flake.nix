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
          name = "oama";
          version = "0.20.0";

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
            mv oama-0.20.0-Linux-x86_64/oama $out/bin/

            mkdir -p $out/share/bash-completion/completions
            mv oama-0.20.0-Linux-x86_64/completions/oama.bash $out/share/bash-completion/completions

            substituteInPlace $out/share/bash-completion/completions/oama.bash --replace /usr/bin/oama $out/bin/oama

            mkdir -p $out/share/doc/oama
            mv oama-0.20.0-Linux-x86_64/configs $out/share/doc/oama

            runHook postInstall
          '';
          src = pkgs.fetchurl {
            url =
              "https://github.com/pdobsan/oama/releases/download/0.20.0/oama-0.20.0-Linux-x86_64.tar.gz";
            sha256 = "sha256-5H2HoRwL/AVsSZnJeS3REH6hNIDj7dTWs5konbEAFgc=";
          };
        };
      });
}
