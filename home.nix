# Bootstrap: git clone git@github.com:konubinix/Devel.git ~/Prog/Devel
# Then: home-manager switch --flake ~/Prog/Devel
# Later on NixOS: nixos-rebuild switch --flake ~/Prog/Devel
{ config, pkgs, lib, ... }:

let
  develDir = "/home/sam/prog/devel";
  configDir = "${develDir}/config";
  shareDir = "${develDir}/share";
in
{
  options.konix.extraPythonPackages = lib.mkOption {
    type = lib.types.functionTo (lib.types.listOf lib.types.package);
    default = _: [];
    description = "Extra Python packages to add to the base Python environment.";
  };

  imports = [
    ./nix/devel-env.nix
    ./nix/devel-bash.nix
    ./nix/devel-emacs.nix
  ];

  config = {
  home.username = "sam";
  home.homeDirectory = "/home/sam";
  home.stateVersion = "24.11";

  programs.home-manager.enable = true;

  home.sessionPath = [
    "${develDir}/bin"
    "${develDir}/bin/hardliases"
    "${config.home.homeDirectory}/.local/bin"
  ];

  home.packages = with pkgs; [
    # editors
    vim
    emacs
    gcc   # needed by emacs native-comp JIT (async compilation invokes gcc driver)

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
    pipx

    # security
    gnupg
    pinentry-gtk2
    apg
    gfshare # gfcombine, gfsplit
    # impass: packaged in flakes/impass, added via flake.nix

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
    (python3.withPackages (ps: [
      ps.evdev
      ps.pip
      ps.ipython
      ps.xdg
      ps.supervisor
    ] ++ (config.konix.extraPythonPackages ps)))

    # misc
    which
    eject
  ];

  # Agents are managed outside NixOS
  services.gpg-agent.enable = false;
  programs.gpg.enable = true;

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
  }; # end config
}
