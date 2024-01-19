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
        node-modules = pkgs.mkYarnPackage {
          name = "node-modules";
          src = ./.;
        };
        deps = [
          (pkgs.stdenv.mkDerivation rec {
            name = "workbox-cli";
            version = "7.0.0";

            dontUnpack = true;
            dontPatch = true;
            dontConfigure = true;
            dontBuild = true;
            doCheck = false;
            buildInputs = [ node-modules ];

            installPhase = ''
              runHook preInstall

              mkdir -p $out/bin
              echo 'exec ${node-modules}/libexec/workbox/node_modules/.bin/workbox "$@"' >> "$out/bin/workbox"
              chmod +x $out/bin/*

              runHook postInstall
            '';
            src = ./.;
          })
        ];
      in {
        packages.node-modules = node-modules;
        packages.default = pkgs.buildEnv {
          name = "workbox-cli";
          paths = with pkgs; deps;
        };
        devShell = pkgs.mkShell { buildInputs = deps; };
      });
}
