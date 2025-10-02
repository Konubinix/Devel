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
      {
        packages.default =
          with pkgs;
          rustPlatform.buildRustPackage (finalAttrs: {
            pname = "hld";
            version = "0.3.0";

            src = fetchFromGitHub {
              owner = "glehmann";
              repo = "hld";
              tag = finalAttrs.version;
              hash = "sha256-6gLm3XaJINo3hy/Tohci3ghzf+Tp5dB33zsfvkitlZg=";
            };

            cargoHash = "sha256-u8PfSfOKRU0XWtRDIrMQLU78JEdetT/ujRthn8aDvg4=";

            meta = {
              description = "Hard Link Deduplicator";
              homepage = "https://github.com/glehmann/hld";
              license = lib.licenses.mit;
              maintainers = [ ];
            };
          });
      }
    );
}
