# Bash configuration for NixOS.
# Replaces config/bashrc + config/shrc for the NixOS environment.
# On Debian, config/bashrc is still used via install.py.
{ config, lib, pkgs, ... }:

let
  homeDir = config.home.homeDirectory;
  develDir = "/home/sam/prog/devel";
  configDir = "${develDir}/config";
  libDir = "${develDir}/lib";

  sessionVarsFile = pkgs.writeText "hm-session-vars.sh" (
    lib.concatStringsSep "\n" (lib.mapAttrsToList
      (name: value: "export ${name}=${lib.escapeShellArg value}")
      config.home.sessionVariables)
  );
in
{
  home.packages = with pkgs; [
    bash-preexec
    mcfly
    mcfly-fzf
    sourceHighlight
  ];

  # Native HM integrations (replace manual eval hooks)
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

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
      export HM_SESSION_VARS="${sessionVarsFile}"
      source "${sessionVarsFile}"

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
}
