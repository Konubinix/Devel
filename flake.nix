{
  description = "Konubinix' tools";

  # see https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html
  # github:NixOS/nixpkgs/a3a3dda3bacf61e8a39258a0ed9c924eeca8e293: A specific revision of the nixpkgs repository.
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/21.11";

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
      packages.x86_64-linux.mysetup = pkgs.buildEnv {
        name = "mysetup";
        paths = with pkgs; [ nixfmt rnix-lsp ];
      };
    };
}
