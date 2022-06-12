{
  description = "Konubinix' tools";

  # see https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html
  # github:NixOS/nixpkgs/a3a3dda3bacf61e8a39258a0ed9c924eeca8e293: A specific revision of the nixpkgs repository.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/21.11";

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
      packages.x86_64-linux.devel = pkgs.buildEnv {
        name = "devel";
        paths = with pkgs; [ nixfmt rnix-lsp direnv xh ];
      };
      packages.x86_64-linux.k8s = pkgs.buildEnv {
        name = "k8s";
        paths = with pkgs; [ kubie (callPackage ./nix/kubecolor { }) ];
      };
      packages.x86_64-linux.hashicorp = pkgs.buildEnv {
        name = "hashicorp";
        paths = with pkgs; [ terraform-ls ];
      };
      packages.x86_64-linux.threed-print = pkgs.buildEnv {
        name = "3dprint";
        paths = with pkgs; [ slic3r ];
      };
      packages.x86_64-linux.nodejs = pkgs.buildEnv {
        name = "nodejs";
        paths = with pkgs; [ node2nix ];
      };
    };
}
