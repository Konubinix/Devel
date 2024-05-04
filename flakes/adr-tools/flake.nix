{
  description = "adr-tools";

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.env = pkgs.buildEnv {
          name = "adr-tools";
          paths = with pkgs; [ packages.default ];
        };
        packages.default = pkgs.stdenv.mkDerivation rec {
          name = "adr-tools";
          version = "3.0.0";

          dontUnpack = true;
          dontPatch = true;
          dontConfigure = true;
          dontBuild = true;
          doCheck = false;
          dontPatchShebangs = 1;

          installPhase = ''
            runHook preInstall

            mkdir -p $out/
            cp -r $src/src $out/bin
            chmod +w $out/bin
            cp $out/bin/_adr_autocomplete $out/bin/_adr_autocomplete2
            chmod +x $out/bin/*


            mkdir -p $out/share/bash-completion/completions/
            cp $src/autocomplete/adr $out/share/bash-completion/completions/

            runHook postInstall
          '';
          src = pkgs.fetchFromGitHub {
            owner = "npryce";
            repo = name;
            rev = "${version}";
            sha256 = "sha256-JEwLn+SY6XcaQ9VhN8ARQaZc1zolgAJKfIqPggzV+sU=";
          };
        };
      });
}
