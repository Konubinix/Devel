{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
  };

  outputs = { self, nixpkgs, flake-utils, nix-npm-buildpackage, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        argdown =
          (nix-npm-buildpackage.legacyPackages.${system}.buildNpmPackage {
            src = ./.;
          });
        deps = [
          (pkgs.stdenv.mkDerivation rec {
            name = "argdown";
            version = "0.1.0";

            dontUnpack = true;
            dontPatch = true;
            dontConfigure = true;
            dontBuild = true;
            doCheck = false;
            buildInputs = [ argdown ];

            installPhase = ''
              runHook preInstall

              mkdir -p $out/bin
              echo 'exec ${argdown}/node_modules/.bin/argdown "$@"' >> "$out/bin/argdown"
              chmod +x $out/bin/*

              runHook postInstall
            '';
            src = ./.;
          })
        ];
      in {
        packages.default = pkgs.buildEnv {
          name = "argdown";
          paths = with pkgs; deps;
        };
        devShell = pkgs.mkShell { buildInputs = deps; };
      });
}
