# used saxon build from /nix/store/yz0w1s863vqsas7jpzg5rpc29ig17566-source/pkgs/development/libraries/java/saxon/default.nix as base
{
  description = "Studio";

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        deps = [ ];
      in rec {
        packages.default = pkgs.stdenv.mkDerivation rec {
          pname = "studio";
          version = "0.3.1";
          jre = pkgs.openjdk11;
          name = "${pname}-${version}";
          src = pkgs.fetchurl {
            url =
              "https://github.com/marian-m12l/studio/releases/download/0.3.1/studio-web-ui-0.3.1-dist.zip";
            sha256 = "sha256-DRufOCj7OjP0NaGuJjjb+9fZvr8wE56wHVQ/zhlIFEc=";
          };
          nativeBuildInputs = [
            pkgs.unzip # pkgs.breakpointHook
          ];
          prog = "lunii-studio";
          buildCommand = ''
            unzip $src -d $out
            mkdir -p $out/bin
            ls $out
            sed -i "s|^java|export JAVA_HOME=${jre}\n${jre}/bin/java|" $out/studio-web-ui-0.3.1/studio-linux.sh
            cat > $out/bin/${prog} <<EOF
            #! $shell
            export JAVA_HOME=${jre}
            exec $out/studio-web-ui-0.3.1/studio-linux.sh
            EOF
            chmod a+x $out/bin/${prog}
          '';

          meta = with pkgs.lib; {
            description = "STUdio - Story Teller Unleashed";
            licence = licenses.mpl20;
            homepage = "https://github.com/marian-m12l/studio";
            sourceProvenance = with sourceTypes; [ binaryBytecode ];
            platforms = platforms.all;
          };
        };
      });
}
