{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        voice = "https://huggingface.co/rhasspy/piper-voices/resolve/v1.0.0/fr/fr_FR/siwis/medium/fr_FR-siwis-medium";
        model = pkgs.fetchurl {
          url = "${voice}.onnx";
          sha256 = "16dg7fkn8al02hc3iqwbrkg8bcsjn3ni0s07ih982aysjyq1l7b4";
        };
        config = pkgs.fetchurl {
          url = "${voice}.onnx.json";
          sha256 = "0nc9vzj3lqy0frlhr2dd4gh86kbl1kfsskbnr5d2n6fvq8b9jirr";
        };
        # piper looks for the config next to the model, named <model>.json.
        voiceDir = pkgs.linkFarm "fr_FR-siwis-medium" {
          "voice.onnx" = model;
          "voice.onnx.json" = config;
        };
      in
      {
        # say IN OUT: speak the French text of file IN into the wav OUT.
        packages.default = pkgs.writeShellApplication {
          name = "say";
          runtimeInputs = [ pkgs.piper-tts ];
          text = ''
            piper -m ${voiceDir}/voice.onnx -f "$2" < "$1"
          '';
        };
      }
    );
}
