{ pkgs, ... }:
{
  environment.systemPackages = [
    (pkgs.rustPlatform.buildRustPackage {
      pname = "voyager-disco";
      version = "1753694";
      src = pkgs.fetchFromGitHub {
        owner = "monorkin";
        repo = "voyager-disco";
        rev = "1753694a0d9d43b4bfb56ad5ae362d47beb8538b";
        hash = "sha256-odtKWWb6cfkCCZma/Vpt+o2yuVLFgH2rto3TitnDWqM=";
      };
      cargoHash = "sha256-+AOUfeOvfNzQjqdJjxwHvLgSn4deddaHnEPB5WGCWrY=";
      postPatch = ''
        substituteInPlace src/device.rs \
          --replace-fail "EXPECTED_PROTOCOL_VERSION: u8 = 0x04" "EXPECTED_PROTOCOL_VERSION: u8 = 0x05"
      '';
      nativeBuildInputs = [ pkgs.pkg-config ];
      buildInputs = [ pkgs.udev ];
    })
  ];
}
