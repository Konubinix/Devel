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
          name = "blender";
          version = "3.3.1 LTS";

          dontUnpack = true;
          dontPatch = true;
          dontConfigure = true;
          dontBuild = true;
          doCheck = false;

          installPhase = ''
            runHook preInstall

            mkdir -p $out/bin
            tar -C $out -xf ${src}
            ln -s $out/blender-3.3.1-linux-x64/blender $out/bin/blender
            chmod +x $out/bin/*

            runHook postInstall
          '';
          src = pkgs.fetchurl {
            url =
              "https://ftp.halifax.rwth-aachen.de/blender/release/Blender3.3/blender-3.3.1-linux-x64.tar.xz";
            sha256 = "sha256-MImkhd1iF4XXpwIImrpy0HuPczo2LpAewUSbmjeVRvI=";
          };
        };
      });
}
