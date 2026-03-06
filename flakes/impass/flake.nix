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
      in
      {
        packages.default = impass;
        # Use impass's own wrapped Python which has all deps (gpg, pygobject, etc.)
        impassPython = "${impass}/bin/.impass-wrapped";
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
