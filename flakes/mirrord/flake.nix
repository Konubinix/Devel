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
          name = "mirrord";
          version = "3.107.0";

          dontUnpack = true;
          dontPatch = true;
          dontConfigure = true;
          dontBuild = true;
          doCheck = false;
          buildInputs = [ pkgs.unzip ];

          installPhase = ''
            runHook preInstall

            mkdir -p $out/bin
            pushd $out/bin
            unzip ${src}
            popd
            chmod +x $out/bin/*

            runHook postInstall
          '';
          src = pkgs.fetchurl {
            url =
              "https://github.com/metalbear-co/mirrord/releases/download/3.107.0/mirrord_linux_x86_64.zip";
            sha256 = "sha256-v56tr8macbrOdti80XY90kvNPo/HUtWUGrT/L+fzhKU=";
          };
        };
      });
}
