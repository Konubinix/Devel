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
          name = "bws";
          version = "0.5.0";

          dontUnpack = true;
          dontPatch = true;
          dontConfigure = true;
          dontBuild = true;
          doCheck = false;
          buildInputs = [ pkgs.unzip ];

          installPhase = ''
            runHook preInstall

            unzip "${src}"

            mkdir -p $out/bin
            mv bws $out/bin/

            runHook postInstall
          '';
          src = pkgs.fetchurl {
            url =
              "https://github.com/bitwarden/sdk/releases/download/bws-v0.5.0/bws-x86_64-unknown-linux-gnu-0.5.0.zip";
            sha256 = "sha256-uSljQVSdm6aSLaZpKyTE2B0U3DmSWX1ad3aSruc7ELI=";
          };
        };
      });
}
