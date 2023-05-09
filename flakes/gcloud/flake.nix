{
  description = "golang env";

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        deps = [
          (pkgs.google-cloud-sdk.withExtraComponents
            ([ pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin ]))
        ];
      in {
        packages.default = pkgs.buildEnv {
          name = "go";
          paths = with pkgs; deps;
        };
        devShell = pkgs.mkShell { buildInputs = deps; };
      });
}
