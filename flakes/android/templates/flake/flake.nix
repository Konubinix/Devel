{
  description = "Android env";

  inputs = {
    konubinix-android.url = "/home/sam/Prog/devel/flakes/android";
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, konubinix-android, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = [ konubinix-android.devShell.${system}.buildInputs ];
          shellHook = konubinix-android.devShell.${system}.shellHook;
        };
      });
}
