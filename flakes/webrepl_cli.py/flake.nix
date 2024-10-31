{
  description = "webrepl_cli";

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.env = pkgs.buildEnv {
          name = "webrepl_cli";
          paths = with pkgs; [ packages.default ];
        };
        packages.default = pkgs.stdenv.mkDerivation rec {
          name = "webrepl_cli";
          version = "0.0.0";

          dontUnpack = true;
          dontPatch = true;
          dontConfigure = true;
          dontBuild = true;
          doCheck = false;
          dontPatchShebangs = 1;

          installPhase = ''
            runHook preInstall

            mkdir -p $out/bin
            cp -r $src/webrepl_cli.py $out/bin
            chmod +w $out/bin

            runHook postInstall
          '';
          src = pkgs.fetchFromGitHub {
            owner = "micropython";
            repo = "webrepl";
            rev = "1e09d9a1d90fe52aba11d1e659afbc95a50cf088";
            sha256 = "sha256-fUewic89i1TeQWLH66Bbic37KIgwtgPDLsYH1xKpExY=";
          };
        };
      });
}
