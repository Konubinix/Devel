{
  description = "msmtpq";

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.env = pkgs.buildEnv {
          name = "msmtpq";
          paths = with pkgs; [ packages.default ];
        };
        packages.default = pkgs.stdenv.mkDerivation rec {
          name = "msmtp-queue";
          version = "0.1.0";

          dontUnpack = true;
          dontPatch = true;
          dontConfigure = true;
          dontBuild = true;
          doCheck = false;

          installPhase = ''
            runHook preInstall

            mkdir -p $out/bin
            cp $src/msmtpq $src/msmtpq-flush $out/bin
            chmod +x $out/bin/*

            runHook postInstall
          '';
          src = pkgs.fetchFromGitHub {
            owner = "Stebalien";
            repo = name;
            rev = "refs/heads/master";
            sha256 = "sha256-Ab23+aWvuGv5s+FUoZGCv4gC0RYQEA7pene+b7nw8fw=";
          };
        };
      });
}
