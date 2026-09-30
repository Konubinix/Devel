{ pkgs, ... }:
{
  systemd.user.services.keymapp = {
    description = "Keymapp, which kontroll talks to";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = "${pkgs.keymapp}/bin/keymapp";
      Restart = "on-failure";
    };
  };
}
