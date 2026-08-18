{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  pname = "consul";
  version = "1.22.7";

  src =
    let
      inherit (pkgs.stdenv.hostPlatform) system;
      selectSystem = attrs: attrs.${system} or (throw "Unsupported system: ${system}");
      suffix = selectSystem {
        x86_64-linux = "linux_amd64";
      };
      hash = selectSystem {
        x86_64-linux = "sha256-BF8cN7zvQN/KBOvkOPnGrodp9htHXBoN9Vh5KjaaEJc=";
      };
    in
    pkgs.fetchzip {
      url = "https://releases.hashicorp.com/consul/${version}/consul_${version}_${suffix}.zip";
      stripRoot = false;
      inherit hash;
    };

  dontConfigure = true;
  dontBuild = true;

  # statically linked Go binary
  dontStrip = true;
  dontPatchELF = true;
  dontPatchShebangs = true;

  installPhase = ''
    runHook preInstall
    install -D consul $out/bin/consul
    runHook postInstall
  '';

  doInstallCheck = true;
  installCheckPhase = ''
    runHook preInstallCheck
    $out/bin/consul version
    runHook postInstallCheck
  '';

  meta = {
    description = "Tool for service discovery, monitoring and configuration";
    homepage = "https://www.consul.io/";
    sourceProvenance = with pkgs.lib.sourceTypes; [ binaryNativeCode ];
    license = pkgs.lib.licenses.bsl11;
    mainProgram = "consul";
    platforms = [ "x86_64-linux" ];
  };
}
