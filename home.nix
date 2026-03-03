# Bootstrap: git clone git@github.com:konubinix/Devel.git ~/Prog/Devel
# Then: home-manager switch --flake ~/Prog/Devel
# Later on NixOS: nixos-rebuild switch --flake ~/Prog/Devel
{ config, pkgs, ... }:

let
  develDir = "/home/sam/Prog/Devel";
in
{
  home.username = "sam";
  home.homeDirectory = "/home/sam";
  home.stateVersion = "24.11";

  programs.home-manager.enable = true;
}
