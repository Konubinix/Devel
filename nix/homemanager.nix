# Bootstrap: git clone git@github.com:konubinix/Devel.git ~/Prog/Devel
# Then: home-manager switch --flake ~/Prog/Devel
# Later on NixOS: nixos-rebuild switch --flake ~/Prog/Devel
{
  config,
  pkgs,
  lib,
  ...
}:

let
  homeDir = config.home.homeDirectory;
  develDir = "/home/sam/prog/devel";
  configDir = "${develDir}/config";
  shareDir = "${develDir}/share";
  libDir = "${develDir}/lib";

  # Wrap a package's binaries so that LD_LIBRARY_PATH includes NIX_LD_LIBRARY_PATH.
  # This lets pip-installed native extensions (numpy, etc.) find libstdc++ and friends
  # without polluting LD_LIBRARY_PATH globally (which breaks Nix-built binaries).
  # See https://bvngee.com/blogs/using-python-virtualenvs-in-nixos
  # Use symlinkJoin so the wrapper output mirrors the original directory structure
  # (including lib/python3.*/site-packages). This lets Python resolve its prefix
  # from the wrapper path and find nix-provided packages (lxml, etc.).
  wrapWithNixLD =
    program:
    pkgs.symlinkJoin {
      name = "${program.pname or program.name}-nix-ld-wrapped";
      paths = [ program ];
      postBuild = ''
              # Replace bin/ symlinks with wrapper scripts that set LD_LIBRARY_PATH
              rm -rf $out/bin
              mkdir -p $out/bin
              for file in ${program}/bin/*; do
                new_file=$out/bin/$(basename $file)
                echo "#! ${pkgs.bash}/bin/bash -e" >> $new_file
                # Save original LD_LIBRARY_PATH so sitecustomize.py can restore it for
                # child processes (prevents nix-ld libs from leaking into nix-built binaries).
                echo 'export _ORIG_LD_LIBRARY_PATH="$LD_LIBRARY_PATH"' >> $new_file
                echo 'export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$NIX_LD_LIBRARY_PATH"' >> $new_file
                # -a "$0" preserves the original argv[0] so Python can detect virtualenvs
                # (pyvenv.cfg lookup). Without it, sys.executable resolves to the nix store
                # binary and venv site-packages are lost.
                echo 'exec -a "$0" '$file' "$@"' >> $new_file
                chmod +x $new_file
              done

              # Add sitecustomize.py that restores LD_LIBRARY_PATH for child processes.
              # The dynamic linker caches search paths at startup, so dlopen still uses
              # the extended paths for loading native extensions (numpy, etc.), but
              # subprocess children get the clean LD_LIBRARY_PATH.
              for sitedir in $out/lib/python*/site-packages; do
                rm -f "$sitedir/sitecustomize.py"
                cat > "$sitedir/sitecustomize.py" << 'PYEOF'
        import os
        _orig = os.environ.pop("_ORIG_LD_LIBRARY_PATH", None)
        if _orig is not None:
            if _orig:
                os.environ["LD_LIBRARY_PATH"] = _orig
            else:
                os.environ.pop("LD_LIBRARY_PATH", None)
        PYEOF
              done
      '';
    };

  # Plain KEY=VALUE file (no shell expansion) for simple session variables.
  sessionVarsFile = pkgs.writeText "hm-session-vars.sh" (
    lib.concatStringsSep "\n" (
      lib.mapAttrsToList (
        name: value: "export ${name}=${lib.escapeShellArg value}"
      ) config.home.sessionVariables
    )
  );

  # KEY=prefix file for variables that need prepending to existing values.
  # Merges sessionPath into PATH automatically.
  sessionPrependVars =
    let
      raw = config.home.sessionPrependVariables;
      sessionPathStr = lib.concatStringsSep ":" config.home.sessionPath;
      pathEntry = if raw ? PATH then sessionPathStr + ":" + raw.PATH else sessionPathStr;
    in
    raw // lib.optionalAttrs (sessionPathStr != "") { PATH = pathEntry; };

  sessionPrependFile = pkgs.writeText "hm-session-prepend-vars.sh" (
    lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: "${name}=${value}") sessionPrependVars)
  );
in
{
  options.konix.extraPythonPackages = lib.mkOption {
    type = lib.types.functionTo (lib.types.listOf lib.types.package);
    default = _: [ ];
    description = "Extra Python packages to add to the base Python environment.";
  };

  options.konix.pythonEnv = lib.mkOption {
    type = lib.types.package;
    readOnly = true;
    description = "The fully built Python environment (with all extra packages).";
  };

  options.home.sessionPrependVariables = lib.mkOption {
    type = lib.types.attrsOf lib.types.str;
    default = { };
    description = "Variables whose values are prepended (colon-separated) to existing values.";
  };

  config = {
    konix.pythonEnv = pkgs.python3.withPackages (
      ps:
      [
        ps.evdev
        ps.pip
        ps.ipython
        ps.xdg
        ps.supervisor
        ps.flake8
        ps.notmuch
        ps.requests
      ]
      ++ (config.konix.extraPythonPackages ps)
    );

    home.username = "sam";
    home.homeDirectory = "/home/sam";
    home.stateVersion = "24.11";

    programs.home-manager.enable = true;

    home.sessionPath = lib.mkAfter [
      "${config.home.homeDirectory}/bin"
      "${config.home.homeDirectory}/.local/bin"
      "${develDir}/bin"
      "${develDir}/bin/hardliases"
    ];

    # ── Packages ──────────────────────────────────────────────────────────

    home.packages = with pkgs; [
      # editors
      vim
      emacs # pgtk is the default since emacs 29+, so emacsclient -c can open graphical frames from emacs --daemon
      gcc # needed by emacs native-comp JIT (async compilation invokes gcc driver)

      # vcs
      gitFull # to also have gitk

      # mail
      notmuch

      offlineimap

      # containers
      docker

      # shell tools
      direnv
      nix-direnv
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
      bash-preexec
      mcfly
      mcfly-fzf
      mosh
      sourceHighlight

      # security
      gnupg
      pinentry-gtk2
      apg
      gfshare # gfcombine, gfsplit
      # impass: packaged in flakes/impass, added via flake.nix

      # spelling
      (aspellWithDicts (ds: [
        ds.fr
        ds.en
      ]))

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

      # audio / media
      pulseaudio # pactl + libpulse.so for pulsectl (PipeWire-compatible)
      pavucontrol
      pasystray
      ffmpeg
      yt-dlp
      git-annex
      # gmpc did not find the correct name so far

      # python — wrapped so venvs/pipx can find nix-ld libraries (libstdc++, zlib, …)
      (wrapWithNixLD config.konix.pythonEnv)

      # misc
      which
      eject
    ];

    # ── GPG ───────────────────────────────────────────────────────────────

    # Agents are managed outside NixOS
    services.gpg-agent.enable = false;
    programs.gpg.enable = true;

    # ── Window manager ────────────────────────────────────────────────────

    xsession.windowManager.awesome = {
      enable = true;
      luaModules = with pkgs.luaPackages; [
        luarocks
        awesome-wm-widgets
      ];
    };

    xsession.initExtra = ''
      awesome_log_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/awesome"
      mkdir -p "$awesome_log_dir"
      exec > "$awesome_log_dir/stdout.log" 2> "$awesome_log_dir/stderr.log"
    '';

    xdg.configHome = configDir;

    # ── Dotfile symlinks ──────────────────────────────────────────────────

    home.file = {
      ".config".source = config.lib.file.mkOutOfStoreSymlink configDir;
      ".byobu".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/byobu";
      ".inputrc".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/inputrc";
      ".konix_hm-session-vars.sh".source = sessionVarsFile;
      ".konix_hm-session-prepend-vars.sh".source = sessionPrependFile;
    };

    # ── Environment variables ─────────────────────────────────────────────

    # Ensure directories that TMPDIR and others point to exist
    home.activation.createDirs = config.lib.dag.entryAfter [ "writeBoundary" ] ''
      mkdir -p "${homeDir}/tmp"
      mkdir -p "${homeDir}/tmp/sob"
      mkdir -p "${homeDir}/log/err"
      mkdir -p "${homeDir}/ctrl"
    '';

    # Priority 1500: perso (mkDefault=1000) and host (normal=100) can override
    home.sessionVariables = lib.mapAttrs (_: lib.mkOverride 1500) {
      # dagger
      DAGGER_NO_NAG = 1;

      # Bootstrap paths
      KONIX_DEVEL_DIR = develDir;
      KONIX_CONFIG_DIR = configDir;
      KONIX_SHARE_DIR = shareDir;
      KONIX_LIB_DIR = "${develDir}/lib";
      KONIX_BIN_DIR = "${develDir}/bin";
      KONIX_SRC_DIR = "${develDir}/src";
      KONIX_PLATFORM = "linux";
      KONIX_EMACS_CUSTOM_FILE = "${homeDir}/.emacs_custo";
      KONIX_PERSO_DIRS = "${homeDir}/perso";
      KONIX_PERSO_DIR = "${homeDir}/perso/perso";
      KONIX_PERSO_CONFIG_DIR = "${homeDir}/perso/perso/config";

      # Editors
      EDITOR = "edit.sh";
      VISUAL = "edit.sh";
      ALTERNATE_EDITOR = "edaemon";
      SVN_EDITOR = "nano";
      BROWSER = "qutebrowser";

      # Locale
      LANG = "C.UTF-8";
      LC_ALL = "C.UTF-8";

      # Shell / history
      HISTTIMEFORMAT = "%y/%m/%d-%H:%M:%S:";
      FZF_CTRL_R_OPTS = "--exact";
      BKT_TTL = "3m";
      LESS = "-SRMX";
      LESSOPEN = "|source-highlight-esc.sh %s";

      # Dirs
      TMPDIR = "${homeDir}/tmp";
      KONIX_SOB_DIR = "${homeDir}/tmp/sob";
      KONIX_TMP_SHARE_DIR = "${homeDir}/tmp/share";
      KONIX_DOWNLOAD_DIR = "${homeDir}/Downloads";
      KONIX_CRYPTOFF = "${homeDir}/cryptoff";
      KONIX_SSHFS_ROOT = "${homeDir}/machines";
      KONIX_SMB_MOUNT = "${homeDir}/Network";
      KONIX_WATCHER_CTRL_DIR = "${homeDir}/ctrl";
      KONIX_LOG_DIR = "${homeDir}/log";
      KONIX_LOG_ERR_DIR = "${homeDir}/log/err";
      KONIX_REMOTE_QUIET = "1";

      # XDG
      XDG_CONFIG_HOME = configDir;
      XDG_CACHE_HOME = "${homeDir}/.cache";
      XDG_DOWNLOAD_DIR = "${homeDir}/Downloads";
      XDG_LOG_HOME = "${homeDir}/tmp/logs";

      # Build tools
      KONIX_MAKE = "konix_makej.sh";
      KONIX_MAKE_LOAD_LIMIT = "auto";
      KONIX_MAKE_SUDIRS = "build:Build";
      KONIX_CCACHE = "ccache";
      KONIX_CC = "gcc";
      KONIX_CXX = "g++";
      CC = "konix_cc.sh";
      CXX = "konix_cxx.sh";

      # Python
      PYTHONBREAKPOINT = "ipdb.set_trace";
      PYTHONSTARTUP = "";
      VIRTUALENVWRAPPER_PYTHON = "python3";
      PYTHON_BIN = "${pkgs.python3}/bin/python3";
      PYTHON_PATH = "${pkgs.python3}/bin";

      # Go
      GO111MODULE = "auto";
      GOPATH = "${homeDir}/gopath";

      # Rust
      RUST_BACKTRACE = "full";

      # Node
      PNPM_HOME = "${homeDir}/.pnpm-home";
      N_PREFIX = "${homeDir}/n";
      npm_config_prefix = "${homeDir}/.local";
      NODE_PATH = "${homeDir}/.local/lib/node_modules:${homeDir}/.pnpm-home/global/5/node_modules";

      # Ruby
      GEM_HOME = "${homeDir}/.local";

      # Git
      GIT_ALLOW_PROTOCOL = "file:git:http:https:ssh:bzr";
      KONIX_GIT_ANNEX_DIRED_METADATA = "--metadata state=next";
      KONIX_GIT_ANNEX_DIRED_SCAN_DIRECTORY = "1";

      # Config file refs (devel)
      MAILCAPS = "${configDir}/mailcap";
      HYPERMAIL_CONFIG = "${configDir}/hmrc";
      KONIX_SUPERVISORDCONF = "${configDir}/supervisord.conf";
      KONIX_UDISKIE_CONFIG = "${configDir}/udiskie.yml";
      KONIX_DEFAULT_CTAGS_CONFIG = "${configDir}/ctags";
      MERGE_ICS_CONF_FILE = "${configDir}/merge_ics.cfg";
      XCOMPOSEFILE = "${configDir}/XCompose";
      KONIX_CONKYRC = "${configDir}/conky/conkyrc";

      # Mail
      KONIX_MAIL = "konubinixweb@gmail.com";
      KONIX_OFFLINEIMAP_FOLDERS_WITH_TRASH = "0";
      KONIX_TEMP_MAIL_KEEP_MONTHS = "3";

      # Network ports / services
      KONIX_HTTP_SHARE_PORT = "9643";
      KONIX_FTP_SHARE_PORT = "9645";
      KONIX_HTTP_SHARE_DIRECTORY_PORT = "9642";
      KONIX_FTP_SHARE_DIRECTORY_PORT = "9644";
      KONIX_NOTEBOOK_PORT = "9640";
      KONIX_NOTEBOOK_IP = "127.0.0.1";
      KONIX_NBVIEWER_PORT = "9638";
      KONIX_NBVIEWER_IP = "0.0.0.0";
      KONIX_NOTEBOOK_BROWSER = "chromium";
      KONIX_NOTEBOOK_VIRTUALENV = "maths3";
      KONIX_MINIDNS_PORT = "53000";
      PROXYRESOLV_DNS = "127.0.0.1 -p53000";
      KONIX_BEETWEB_EXTRA_ARGUMENTS = "127.0.0.1 8337";
      KONIX_GDBSERVER_CONNECT = "localhost:9999";
      KONIX_LANGTOOL_JAR = "${homeDir}/local/languagetool-commandline.jar";

      # Xapers
      KONIX_XAPERS_ADD_TAGS = "new";
      KONIX_XAPERS_ADD_AUTO_PROMPT = "1";
      KONIX_XAPERS_ADD_AUTO_VIEW = "";
      KONIX_XAPERS_ADD_AUTO_MOVE = "1";

      # Syncplay
      KONIX_SYNCPLAY_SERVER = "192.168.1.2";
      KONIX_SYNCPLAY_ROOM = "room";
      KONIX_SYNCPLAY_EXTRA_CONFIG = "-input file=${homeDir}/.mplayer/fifo";

      # GTK
      GTK_IM_MODULE = "xim";

      # Misc
      wpsetters = "feh";
      QUILT_PATCHES = "patches";
      MANPATH = "${homeDir}/.nix-profile/share/man:";
      LD_LIBRARY_PATH = "${homeDir}/.local/lib";
      PKG_CONFIG_PATH = "";
      KONIX_RIL_TRIES = "3";
      KONIX_RIL_TIMEOUT = "10";

      # Bash completion
      COMP_IWLIST_SCAN = "1";
      COMP_CONFIGURE_HINTS = "1";
      COMP_KNOWN_HOSTS_WITH_HOSTFILE = "1";
      COMP_KNOWN_HOSTS_WITH_AVAHI = "1";
      COMP_TAR_INTERNAL_PATHS = "1";
    };

    # ── Bash ──────────────────────────────────────────────────────────────

    # direnv — config is static in config/direnv/, hook added in initExtra
    # programs.starship — reads config from XDG_CONFIG_HOME/starship.toml (in repo)
    programs.starship.enable = true;
    programs.zoxide.enable = true;

    programs.bash = {
      enable = true;
      enableCompletion = true;

      historyFile = "${homeDir}/.bash_history";
      historyFileSize = 5000000;
      historySize = 5000000;
      historyControl = [ "ignoreboth" ];

      shellOptions = [
        "autocd"
        "globstar"
      ];

      shellAliases = {
        # ls
        ll = "ls -l";
        la = "ls -Al";
        l = "ls -CF";

        # grep
        gi = "grep -i --line-buffered";
        CH = "grep --color -i -n -r";
        CHnr = "grep --color -n -i";

        # git-annex
        ga = "git-annex";
        gaw = "git-annex whereis";
        gaI = "git-annex import";
        gaIn = "git-annex import --deduplicate";
        gag = "git-annex get";
        gad = "git-annex drop";
        gac = "git-annex copy";
        gagc = "git-annex-getcopy.sh";
        gau = "git-annex unlock";
        gal = "git-annex lock";
        gas = "git-annex status";
        gai = "git-annex info";
        gat = "git-annex metadata";
        gavp = "git-annex vpop";
        gavf = "git-annex vfilter";
        gava = "git-annex vadd";
        gavc = "git-annex vcycle";
        gafsck = "git-annex fsck";
        gacfg = "git-annex vicfg";
        gaa = "git-annex add";
        gasync = "git-annex sync";
        gaweb = "git-annex sync";
        gam = "git-annex move";

        # misc
        le = "less";
        ns = "netstat -tupln";
        CD = "cd -P .";
        pud = "pushd";
        pod = "popd";
        makej = "konix_makej.sh";
        ctestj = "konix_ctestj.sh";
        alert = "echo -en \"\\a\"";

        # docker
        dkc = "docker-compose";
        dk = "docker";
        dm = "docker-make";

        # systemd
        scu = "systemctl --user";
        jcu = "journalctl --user";
        ssc = "sudo systemctl";
        sjc = "sudo journalctl";

        # display
        ud = "unset DISPLAY";
        sd = "export DISPLAY=:0";

        # xapers
        xva = "konix_xapers_view_and.sh";
        xv = "xapers view";
        xvf = "konix_xapers_view_fuzzy.sh";

      };

      initExtra = ''
        # Re-export session variables so that `home-manager switch` takes
        # effect in every new shell, not just at login.
        eval "$(direnv hook bash)"

        _hm_load_vars() {
          eval "$(konix_hm_session_env.sh | sed 's/^/export /')"
        }
        _hm_load_vars
        hm-reload() { _hm_load_vars; echo "Reloaded session vars"; }

        # --- from shrc ---

        # stty settings
        stty stop undef
        stty rprnt undef
        stty werase undef

        # source personal shrc if available
        if [ -f "''${KONIX_PERSO_DIR:-}/''${HOSTNAME}/shrc" ]; then
          source "''${KONIX_PERSO_DIR}/''${HOSTNAME}/shrc"
        fi

        # ls color
        if ls --color > /dev/null 2>&1; then
          alias ls='ls --color=auto'
        fi

        # source custom libraries
        source "${libDir}/lib_bash.sh"
        source "${libDir}/soblib.sh"

        # --- from bashrc ---

        set -o emacs

        # lesspipe
        LESSPIPE="$(which lesspipe 2>/dev/null)"
        [ -e "$LESSPIPE" -a -x "$LESSPIPE" ] && eval "$(lesspipe)"

        # xterm title
        case "$TERM" in
          xterm*|rxvt*)
            PROMPT_COMMAND='echo -ne "\033]0;''${USER}@''${HOSTNAME}: ''${PWD/''$HOME/~}\007"'
            ;;
        esac

        # custom bash completions
        OLD_IFS=$IFS
        IFS=$'\n'
        source "${configDir}/bash_completion"
        if [ -f "${develDir}/notmuch/completion/notmuch-completion.bash" ]; then
          source "${develDir}/notmuch/completion/notmuch-completion.bash"
        fi
        IFS=$OLD_IFS

        # luarocks
        if which luarocks > /dev/null 2>&1; then
          eval "$(luarocks path)"
        fi

        # cargo
        if test -e "$HOME/.cargo/env"; then
          source "$HOME/.cargo/env"
        fi

        # --- tmux / notification functions ---

        konix_is_in_tmux () {
          test -n "''${TMUX_PANE}"
        }

        konix_tmux_shows_current_window () {
          local current_index="$(tmux display-message -p '#{window_index}')"
          test "''${KONIX_TMUX_WINDOW}" == "''${current_index}"
        }

        konix_is_screen_on () {
          test -e ~/.here
        }

        konix_wm_shows_byobu () {
          DISPLAY="''${DISPLAY:-:0}" konix_wmctrl_active_window.sh | grep -q byobu
        }

        konix_warn_it_took_too_long () {
          local args=()
          if konix_was_inactive_for_a_very_long_time; then
            args+=(-t boring)
          fi
          local duration_human
          if (( KONIX_DURATION >= 3600 )); then
            duration_human="$((KONIX_DURATION / 3600))h $((KONIX_DURATION % 3600 / 60))m $((KONIX_DURATION % 60))s"
          elif (( KONIX_DURATION >= 60 )); then
            duration_human="$((KONIX_DURATION / 60))m $((KONIX_DURATION % 60))s"
          else
            duration_human="''${KONIX_DURATION}s"
          fi
          clk notify "''${args[@]}" -o "'$(HISTTIMEFORMAT=":" history|tail -1|sed -r 's/^[^:]+:(.+)$/\1/')' took ''${duration_human}"
        }

        konix_was_active_not_long_ago () {
          local last_activity="$(tmux display-message -p '#{client_activity}')"
          local now="$(date +%s)"
          test $(( now - last_activity )) -le 3
        }

        konix_was_inactive_for_a_very_long_time () {
          local last_activity="$(tmux display-message -p '#{client_activity}')"
          local now="$(date +%s)"
          test $(( now - last_activity )) -gt 10
        }

        function konix_warn_if_took_too_long(){
          if test -z "''${KONIX_BASH_WARN_IF_TOO_LONG}"; then
            return
          fi
          if test -n "''${KONIX_DURATION}" && test ''${KONIX_DURATION} -gt 2; then
            if konix_is_in_tmux; then
              if konix_is_screen_on; then
                if ! ( konix_wm_shows_byobu && konix_tmux_shows_current_window ); then
                  konix_warn_it_took_too_long
                fi
              else
                if ! ( konix_tmux_shows_current_window && konix_was_active_not_long_ago ); then
                  konix_warn_it_took_too_long
                fi
              fi
            fi
          fi
        }

        # --- bash-preexec hooks ---
        source "${pkgs.bash-preexec}/share/bash/bash-preexec.sh"

        preexec_konix() {
          KONIX_STARTTIME="$(date +%s)"
          sob_preexec "$1"
        }
        preexec_functions+=(preexec_konix)

        postexec_konix() {
          if test -n "''${KONIX_STARTTIME}"; then
            local endtime="$(date +%s)"
            KONIX_DURATION="$((endtime - KONIX_STARTTIME))"
            konix_warn_if_took_too_long
          fi
          sob_postexec
        }
        precmd_functions+=(postexec_konix)

        # mcfly
        eval "$(mcfly init bash)"
        eval "$(mcfly-fzf init bash)"

        # GPG
        GPG_TTY="$(tty)"
        export GPG_TTY
      '';
    };

    # ── Emacs ─────────────────────────────────────────────────────────────

    # ~/.emacs_var — simplified for NixOS (env vars already in process environment)
    home.file.".emacs_var".text = ''
      ;; -*- mode:emacs-lisp -*-
      ;; NixOS: env vars set by Home Manager, no need for konix_import_env.el
      (defvar python-bin (executable-find "python3"))
      (defvar perso-dir (or (getenv "KONIX_PERSO_DIR") (expand-file-name "perso" "~")))
      (defvar perso-dirs (or (getenv "KONIX_PERSO_DIRS") perso-dir))
      (defvar config-dir (getenv "KONIX_CONFIG_DIR") "where I put my config files")
      (defvar elfiles (expand-file-name "elfiles" (getenv "KONIX_DEVEL_DIR")) "where I put my emacs files")
      (defvar emacs-config-dir (expand-file-name "config" elfiles) "where I put my emacs custom config files")
      (defvar devel-dir (getenv "KONIX_DEVEL_DIR") "Where I put my devel files (the installation path)")
      (setq custom-file (getenv "KONIX_EMACS_CUSTOM_FILE"))
    '';

    # ~/.emacs — main entry point
    home.file.".emacs".text = ''
      (setq debug-on-quit t)
      (setq emacs_com_file (getenv "EMACS_START_COM"))
      (load-file "${homeDir}/.emacs_var")
      (load-file "${configDir}/emacs.el")
      ;; On finit par loader les customs
      (if custom-file
          (load custom-file)
        (display-warning 'No-custom "No custom file found")
        )
      (when emacs_com_file
        (with-temp-buffer
          (insert "ended\n")
          (write-file emacs_com_file)
          )
        )
    '';

    # ~/.emacs_custo — copy default if not present
    home.activation.emacsCustom = config.lib.dag.entryAfter [ "writeBoundary" ] ''
      if [ ! -f "${homeDir}/.emacs_custo" ]; then
        cp "${configDir}/emacs-custom.el" "${homeDir}/.emacs_custo"
      fi
    '';

    # ~/init_bin symlink (used by various scripts, not just emacs)
    home.file."init_bin".source = config.lib.file.mkOutOfStoreSymlink "${develDir}/init_bin";

  }; # end config
}
