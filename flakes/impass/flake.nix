{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      let
        impass = import ./default.nix { inherit pkgs; };
        # impass's own wrapped Python, which has all deps (gpg, pygobject, etc.).
        # Bound here, not just exposed below, so the shellHook can refer to it.
        impassPython = "${impass}/bin/.impass-wrapped";
      in
      {
        packages.default = impass;
        inherit impassPython;
        devShells.default = pkgs.mkShell {
          packages = [ impass ];
          shellHook = ''
            # Alias python3 to impass's Python (has all deps)
            impass-python3() { ${impassPython} "$@"; }
            export -f impass-python3
            echo "Use 'impass-python3 ./konix_impass.py gui' to run with impass deps"
          '';
        };
      }
    );
}
