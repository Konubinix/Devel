{
  description = "konubinix personal environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Consumed as a plain file by nix/nixos.nix, which turns it into the
    # global flake registry. Pinned here so it refreshes on `nix flake
    # update` rather than being refetched whenever tarball-ttl lapses.
    flake-registry = {
      url = "github:NixOS/flake-registry";
      flake = false;
    };
  };

  outputs =
    {
      nixpkgs,
      home-manager,
      flake-registry,
      ...
    }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ (import ./nix/pins-overlay.nix) ];
      };
      impass = import ./flakes/impass/default.nix { inherit pkgs; };
    in
    {
      overlays.pins = import ./nix/pins-overlay.nix;

      # Reusable module for other flakes (e.g. perso.git) to import
      homeManagerModules.default = {
        imports = [ ./nix/homemanager.nix ];
        home.packages = [ impass ];
      };
      nixosModules.default = ./nix/nixos.nix;

      # standalone home-manager (devel-only, no perso)
      homeConfigurations."sam" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          ./nix/homemanager.nix
          { home.packages = [ impass ]; }
        ];
      };

      # NixOS system + home-manager (devel-only, no perso)
      nixosConfigurations."konix" = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = { inherit nixpkgs flake-registry; };
        modules = [
          ./nix/nixos.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.sam = import ./nix/homemanager.nix;
          }
        ];
      };
    };
}
