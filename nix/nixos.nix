# Base NixOS system configuration (devel layer).
{
  config,
  pkgs,
  nixpkgs,
  flake-registry,
  ...
}:

{
  imports = [
    ./hardware-configuration.nix
    ./voyager-ergol.nix
  ];

  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  # Registry: the upstream entries (so `nix run home-manager` and friends
  # keep working) with our own pins layered on top. Upstream targets carry no
  # rev, so unpinned ids are still fetched at use time; pinned ones aren't.
  nix.registry =
    let
      upstream = builtins.fromJSON (
        builtins.readFile "${flake-registry}/flake-registry.json"
      );
    in
    builtins.listToAttrs (
      map (e: {
        name = e.from.id;
        value.to = e.to;
      }) upstream.flakes
    )
    // {
      # Pinned so hardliases (nix profile add nixpkgs#foo) use the same
      # nixpkgs as the system.
      nixpkgs.flake = nixpkgs;
    };

  # Serve the above as the *global* registry rather than fetching
  # channels.nixos.org every time tarball-ttl lapses. This is also what makes
  # the pins reach flakes/*/flake.nix: flake inputs (eg nixpkgs.url =
  # "nixpkgs") resolve against the global registry and skip the system one,
  # so a pin only applies to them if it lives in this file.
  nix.settings.flake-registry = "/etc/nix/registry.json";

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.hostName is set in the host layer (e.g. konixwork/nix/nixos.nix)
  environment.variables.HOSTNAME = config.networking.hostName;

  # Enable networking
  networking.networkmanager.enable = true;

  # System packages
  environment.systemPackages = with pkgs; [
    net-tools # ifconfig, netstat, etc.
    tinc # VPN (configure services.tinc.networks when needed)
    udftools
    (writeShellScriptBin "konix_tty_is_ssh" (builtins.readFile ./scripts/konix_tty_is_ssh.sh))
  ];

  services.udisks2.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Paris";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "fr_FR.UTF-8";
    LC_IDENTIFICATION = "fr_FR.UTF-8";
    LC_MEASUREMENT = "fr_FR.UTF-8";
    LC_MONETARY = "fr_FR.UTF-8";
    LC_NAME = "fr_FR.UTF-8";
    LC_NUMERIC = "fr_FR.UTF-8";
    LC_PAPER = "fr_FR.UTF-8";
    LC_TELEPHONE = "fr_FR.UTF-8";
    LC_TIME = "fr_FR.UTF-8";
  };

  services.xserver = {
    enable = true;

    # Equivalent to: xset r rate 170 150
    # autoRepeatDelay is in ms (same as xset delay)
    # autoRepeatInterval is in ms between repeats (1000 / xset rate = 1000 / 150 ≈ 7)
    autoRepeatDelay = 170;
    autoRepeatInterval = 7;

    # awesome must be declared here so sddm registers the session
    windowManager.awesome.enable = true;

    # SDDM v0.20+ removed UserAuthFile= and hardcodes the auth cookie to
    # /tmp/xauth_XXXXXX (random). That breaks any workflow that needs a stable
    # XAUTHORITY (ssh-in, scripts) and gets nuked when systemd-tmpfiles cleans
    # /tmp after 10 days of no logout. Copy to $HOME/.Xauthority (the legacy
    # default, so FHS wrappers still find it) and re-export.
    # Refs:
    #   https://github.com/sddm/sddm/issues/944
    #   https://gitlab.archlinux.org/archlinux/packaging/packages/sddm/-/work_items/3
    #   https://github.com/NixOS/nixpkgs/issues/239054
    displayManager.sessionCommands = ''
      if [ -n "$XAUTHORITY" ] && [ "$XAUTHORITY" != "$HOME/.Xauthority" ]; then
        install -m 600 "$XAUTHORITY" "$HOME/.Xauthority"
        export XAUTHORITY="$HOME/.Xauthority"
        systemctl --user import-environment XAUTHORITY
      fi
    '';

    xkb = {
      layout = "fr";
      variant = "bepo";
    };
  };

  services.displayManager = {
    sddm.enable = true;
    defaultSession = "none+awesome";
  };

  # Derive the console keymap from services.xserver.xkb (fr/bepo) so the layout
  # is configured in exactly one place. Must not set console.keyMap alongside it.
  console.useXkbConfig = true;

  # Docker
  virtualisation.docker.enable = true;

  # Define a user account. Don't forget to set a password with 'passwd'.
  users.users.sam = {
    isNormalUser = true;
    description = "sam";
    extraGroups = [
      "dialout"
      "docker"
      "networkmanager"
      "wheel"
    ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Fonts
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-color-emoji
  ];

  # Audio (PipeWire with PulseAudio compatibility)
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable atd for scheduled jobs (at, atq, atrm)
  services.atd.enable = true;

  programs = {
    # FUSE
    # enable is required: the whole module body is mkIf cfg.enable, so without it
    # userAllowOther is inert and no setuid fusermount wrapper is created.
    fuse = {
      enable = true;
      userAllowOther = true;
    };

    # Agents are managed outside NixOS
    ssh.startAgent = false;
    gnupg.agent.enable = false;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11";
}
