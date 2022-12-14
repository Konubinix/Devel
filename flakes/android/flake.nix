{
  description = "Android dev env";

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
          deps = [ pkgs.androidenv.androidPkgs_9_0.platform-tools ];
      in {
        packages.default = pkgs.buildEnv {
          name = "android";
          paths = with pkgs; deps;
        };
        devShell = pkgs.mkShell {
          buildInputs = deps;
        };
      });
}
