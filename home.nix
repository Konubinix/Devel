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
  imports = [
    ./nix/devel-env.nix
    ./nix/devel-bash.nix
    ./nix/devel-emacs.nix
    # ./nix/perso-env.nix  # from perso.git, uncomment when ready
    # ./nix/host-env.nix   # from host.git, uncomment when ready
  ];
  home.username = "sam";
  home.homeDirectory = "/home/sam";
  home.stateVersion = "24.11";

  programs.home-manager.enable = true;

  home.sessionPath = [
    "${develDir}/bin"
    "${develDir}/bin/hardliases"
  ];

  home.packages = with pkgs; [
    # editors
    vim
    emacs

    # vcs
    git
    git-annex

    # mail
    notmuch
    python3Packages.notmuch
    offlineimap

    # containers
    docker

    # shell tools
    bc
    byobu
    curl
    expect # unbuffer
    fzf
    htop
    jq
    moreutils # sponge
    rsync
    unzip
    autojump
    fish

    # security
    gnupg
    pinentry-gtk2
    apg
    gfshare # gfcombine, gfsplit
    # impass # check if available in nixpkgs

    # spelling
    aspell
    aspellDicts.fr
    aspellDicts.en

    # OCR
    tesseract
    # tessdata (eng+fra included by default)

    # X11 tools
    wmctrl
    xdotool
    xscreensaver
    xterm
    dmenu # suckless-tools equivalent
    eog
    peek
    pcmanfm
    libnotify # notify-send

    # terminal
    terminator

    # browsers
    chromium

    # audio
    pavucontrol
    pasystray
    # gmpc did not find the correct name so far

    # python
    python3
    python3Packages.pip
    python3Packages.ipython
    python3Packages.pyxdg

    # misc
    which
    eject
    python3Packages.supervisor
  ];

  xsession.windowManager.awesome = {
    enable = true;
    luaModules = with pkgs.luaPackages; [
      luarocks
      awesome-wm-widgets
    ];
  };

  # Dotfile symlinks (replaces install.py substitute() calls)
  home.file = {
    ".config/awesome".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/awesome";
    ".byobu".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/byobu";
    ".inputrc".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/inputrc";
    ".config/starship.toml".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/starship.toml";
  };
}
