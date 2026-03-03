{
  description = "konubinix personal environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      # Reusable module for other flakes (e.g. perso.git) to import
      homeManagerModules.default = ./home.nix;
      nixosModules.default = ./nixos/configuration.nix;

      # standalone home-manager (devel-only, no perso)
      homeConfigurations."sam" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [ ./home.nix ];
      };

      # NixOS system + home-manager (devel-only, no perso)
      nixosConfigurations."konixos" = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./nixos/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.sam = import ./home.nix;
          }
        ];
      };
    };
}
