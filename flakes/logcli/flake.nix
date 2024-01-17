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
          name = "logcli";
          version = "2.8.7";

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
            ln -sr logcli-linux-amd64 logcli
            popd
            chmod +x $out/bin/*

            runHook postInstall
          '';
          src = pkgs.fetchurl {
            url =
              "https://github.com/grafana/loki/releases/download/v2.8.7/logcli-linux-amd64.zip";
            sha256 = "sha256-Kxl8EBNY8ROeAsQlh7d5dfU35lFlQGu2qhdDA+JAC90=";
          };
        };
      });
}
