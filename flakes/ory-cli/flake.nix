{
  description = "Ory CLI";

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.env = pkgs.buildEnv {
          name = "ory-cli";
          paths = with pkgs; [ packages.default ];
        };
        packages.default = pkgs.buildGoModule rec {
          pname = "ory-cli";
          version = "0.2.2";
          rev = "v${version}";
          src = pkgs.fetchurl {
            url = "https://github.com/ory/cli/archive/refs/tags/${rev}.tar.gz";
            hash = "sha256-Wv/0HOYshUl+eEsdQvXjftcu7pDISOTLQrsc+RQe6RE=";
          };
          checkPhase = "";
          vendorHash = "sha256-J9jyeLIT+1pFnHOUHrzmblVCJikvY05Sw9zMz5qaDOk=";
          postInstall = ''
            mv $out/bin/{cli,ory}
            mv $out/bin/{clidoc,orydoc}
            mv $out/bin/{e2e,orye2e}
          '';
        };
      });
}
