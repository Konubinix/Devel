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
          name = "kubectl-node_shell";
          version = "dev";

          dontUnpack = true;
          dontPatch = true;
          dontConfigure = true;
          dontBuild = true;
          doCheck = false;

          installPhase = ''
            runHook preInstall

            mkdir -p $out/bin
            cp ${src} $out/bin/$name
            chmod +x $out/bin/*

            runHook postInstall
          '';
          src = pkgs.fetchurl {
            url =
              "https://raw.githubusercontent.com/kvaps/kubectl-node-shell/e4e79eefc56be52813e115bf9f17c896d65e6e22/kubectl-node_shell";
            sha256 = "sha256-9Ua3VC/sue8nDl33CIKSIvCXnChDeESXfxSXVDSCJtw=";
          };
        };
      });
}
