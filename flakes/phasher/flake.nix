{
  description =
    "phasher (from stash https://docs.stashapp.cc/beginner-guides/guide-to-scraping/)";

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.env = pkgs.buildEnv {
          name = "phasher";
          paths = with pkgs; [ packages.default ];
        };
        packages.default = pkgs.buildGoModule rec {
          pname = "phasher";
          version = "0.25.1";
          rev = "v${version}";
          src = pkgs.fetchFromGitHub {
            owner = "stashapp";
            repo = "stash";
            rev = rev;
            sha256 = "sha256-ssSYd+wAabRrLRxNj2wO6AkN44uKgoZLHUuhZjzkmhs=";
          };

          subPackages = [ "./cmd/phasher" ];

          vendorHash = "sha256-s+BJtxq/lr81Gssk7q+eN+pZmra/7Xdgxp8Irkz3Nyo=";
          postInstall = "\n  \n  ";
        };
      });
}
