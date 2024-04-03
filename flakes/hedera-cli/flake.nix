{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        pname = "hedera-cli";
        version = "0.1.2";
        hederacli = (pkgs.buildNpmPackage {
          name = "hedera";
          src = pkgs.fetchFromGitHub {
            owner = "hashgraph";
            repo = pname;
            rev = "v${version}";
            hash = "sha256-P7MS9zIW7MXZe6vZNPT5a9JawA+S2epcpInN7Tdhe/Q=";
          };
          npmDepsHash = "sha256-fA+/JmEqT4yCtnX8Ncwn0vBmSlnbx1TuM9oLL8+Dk1M=";
          installPhase = ''
          runHook preInstall

          mkdir -p $out/lib/          
          cp -r dist $out/lib/
          cp -r node_modules $out/lib/

          mkdir -p $out/bin
          echo 'exec ${pkgs.nodejs}/bin/node ' "$out/lib/dist/hedera-cli.js" '"$@"' >> "$out/bin/hedera-cli"
          chmod +x $out/bin/*
                    
          runHook postInstall
          '';         
        });
        deps = [ hederacli ];
      in {
        packages.hedera-cli = hederacli;
        packages.default = pkgs.buildEnv {
          name = "hedera-cli";
          paths = with pkgs; deps;
        };
        devShell = pkgs.mkShell { buildInputs = deps; };
      });
}
