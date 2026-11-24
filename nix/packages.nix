{
  config,
  pkgs,
  ...
}:

let
  # Wrap a package's python interpreters so that LD_LIBRARY_PATH includes NIX_LD_LIBRARY_PATH.
  # This lets pip-installed native extensions (numpy, etc.) find libstdc++ and friends
  # without polluting LD_LIBRARY_PATH globally (which breaks Nix-built binaries).
  # See https://bvngee.com/blogs/using-python-virtualenvs-in-nixos
  # Use symlinkJoin so the wrapper output mirrors the original directory structure
  # (including lib/python3.*/site-packages). This lets Python resolve its prefix
  # from the wrapper path and find nix-provided packages (lxml, etc.).
  allowNativePipWheels =
    program:
    pkgs.symlinkJoin {
      name = "${program.pname or program.name}-nix-ld-wrapped";
      paths = [ program ];
      postBuild = ''
              # Replace the python* symlinks with wrapper scripts that set LD_LIBRARY_PATH.
              # Other entry points run the bare store python, which never loads the
              # sitecustomize.py below, so wrapping them would leak into their children.
              rm -rf $out/bin
              mkdir -p $out/bin
              for file in ${program}/bin/*; do
                new_file=$out/bin/$(basename $file)
                case "$(basename $file)" in
                  python*) ;;
                  *)
                    ln -s $file $new_file
                    continue
                    ;;
                esac
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
in
{
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
    sourceHighlight

    # security
    gnupg
    pinentry-gnome3
    apg
    gfshare # gfcombine, gfsplit
    (import ../flakes/impass/default.nix { inherit pkgs; })

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
    # gmpc did not find the correct name so far

    # python — wrapped so venvs/pipx can find nix-ld libraries (libstdc++, zlib, …)
    (allowNativePipWheels config.konix.pythonEnv)

    # misc
    which
    eject
  ];
}
