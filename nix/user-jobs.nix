# Long-running user jobs, started with the graphical session.
{
  config,
  lib,
  pkgs,
  ...
}:

let
  develDir = config.konix.develDir;

  # Same environment as the login shells.
  run =
    name: command:
    pkgs.writeShellScript "konix-job-${name}" ''
      set -a
      eval "$(${develDir}/bin/konix_hm_session_env.sh)"
      set +a
      exec ${command}
    '';
in
{
  options.konix.userJobs = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          command = lib.mkOption { type = lib.types.str; };
          autostart = lib.mkOption {
            type = lib.types.bool;
            default = true;
          };
        };
      }
    );
    default = { };
  };

  config = {
    systemd.user.services = lib.mapAttrs (
      name: job:
      {
        Unit = {
          Description = name;
          PartOf = [ "graphical-session.target" ];
          After = [ "graphical-session.target" ];
        };
        Service = {
          ExecStart = toString (run name job.command);
          Restart = "on-failure";
        };
      }
      // lib.optionalAttrs job.autostart { Install.WantedBy = [ "graphical-session.target" ]; }
    ) config.konix.userJobs;

    konix.userJobs = {
      appium.command = "android_runner appium";
      away_watch.command = "konix_away_watch.sh";
      qutebrowser_cookies.command = ''clk watchexec --no-notify "$XDG_DATA_HOME/qutebrowser/webengine/Cookies" qutebrowser_dump_cookies.sh'';
      deskflow.command = "deskflow-core server -f --debug INFO --name konix --enable-crypto --address :24800 -c ${develDir}/config/Deskflow/deskflow-server.conf --tls-cert ${develDir}/config/Deskflow/tls/deskflow.pem";
      pasystray.command = "pasystray";
      false_dunst = {
        command = "xvfb-run -a dunst";
        autostart = false;
      };
    };
  };
}
