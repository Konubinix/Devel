{
  description = "Docker Scout CLI plugin";

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.default = pkgs.stdenv.mkDerivation rec {
          pname = "docker-scout";
          version = "1.24.0";

          src = pkgs.fetchurl {
            url =
              "https://github.com/docker/scout-cli/releases/download/v${version}/docker-scout_${version}_linux_amd64.tar.gz";
            sha256 = "sha256-9OKBS9YQQDZRU9W5ZLFEyy3G7lNqaLW6xMrfAPwOw0s=";
          };
          sourceRoot = ".";
          nativeBuildInputs = [ pkgs.autoPatchelfHook ];
          installPhase = ''
            install -Dm755 docker-scout $out/lib/docker/cli-plugins/docker-scout
            mkdir -p $out/bin
            ln -s $out/lib/docker/cli-plugins/docker-scout $out/bin/docker-scout
          '';
        };
      });
}
