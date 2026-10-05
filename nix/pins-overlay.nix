final: prev:
prev.lib.mapAttrs' (
  file: _:
  let
    name = prev.lib.removeSuffix ".json" file;
    pin = builtins.fromJSON (builtins.readFile ./pins/${file});
  in
  prev.lib.nameValuePair name
    (import (builtins.fetchTree pin.nixpkgs) {
      system = prev.stdenv.hostPlatform.system;
    }).${name}
) (builtins.readDir ./pins)
