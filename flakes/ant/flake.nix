{
  description = "ant, the CLI for the Claude API";

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
        packages.default = pkgs.buildGoModule rec {
          pname = "ant";
          version = "2.4.0";

          src = pkgs.fetchFromGitHub {
            owner = "anthropics";
            repo = "anthropic-cli";
            rev = "v${version}";
            hash = "sha256-z4hffq8w8sk6Y5QbSOq+iNQ4EvdR1HgxvcOqf+Y9kZY=";
          };

          vendorHash = "sha256-PjkikbzVcr25r5NKWyrTLSEnuwkLasUna+Z+M1cZl1E=";

          subPackages = [ "cmd/ant" ];
          env.CGO_ENABLED = 0;
          ldflags = [
            "-s"
            "-w"
            "-X main.version=${version}"
          ];

          meta = with pkgs.lib; {
            mainProgram = "ant";
            description = "The CLI for the Claude API";
            homepage = "https://github.com/anthropics/anthropic-cli";
            license = licenses.asl20;
          };
        };
      }
    );
}
