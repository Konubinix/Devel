# Bootstrap: git clone git@github.com:konubinix/Devel.git ~/Prog/Devel
# Then: home-manager switch --flake ~/Prog/Devel
# Later on NixOS: nixos-rebuild switch --flake ~/Prog/Devel
{ config, pkgs, ... }:

let
  develDir = "/home/sam/prog/devel";
  configDir = "${develDir}/config";
  shareDir = "${develDir}/share";
in
{
  home.username = "sam";
  home.homeDirectory = "/home/sam";
  home.stateVersion = "24.11";

  programs.home-manager.enable = true;

  # Dotfile symlinks (replaces install.py substitute() calls)
  home.file = {
    ".byobu".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/byobu";
    ".inputrc".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/inputrc";
    ".config/starship.toml".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/starship.toml";
  };
}
