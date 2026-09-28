{
  description = "Claude Desktop (official Anthropic Linux .deb), amd64";

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
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        version = "1.17377.1";
        src = pkgs.fetchurl {
          url = "https://downloads.claude.ai/claude-desktop/apt/stable/pool/main/c/claude-desktop/claude-desktop_${version}_amd64.deb";
          sha256 = "f4bd78545200877b591179838de7ad7a577df6ed2e845969dd25690efc5c85c7";
        };

        unpacked = pkgs.stdenvNoCC.mkDerivation {
          pname = "claude-desktop-unpacked";
          inherit version src;
          nativeBuildInputs = [
            pkgs.dpkg
            pkgs.gnutar
          ];
          # tar without -p: drops chrome-sandbox's setuid bit (unused with
          # --no-sandbox), which the nix build sandbox forbids setting.
          unpackPhase = "dpkg-deb --fsys-tarfile $src | tar -x";
          installPhase = "mkdir -p $out && cp -r usr/* $out/";
          dontPatchELF = true;
          dontStrip = true;
          dontFixup = true;
        };

        # Cowork's VM looks for OVMF firmware at the Debian path
        # /usr/share/OVMF/OVMF_CODE.fd; remap nixpkgs' layout to match.
        ovmfDebian = pkgs.runCommand "ovmf-debian-layout" { } ''
          mkdir -p $out/share/OVMF
          cp ${pkgs.OVMF.fd}/FV/OVMF_CODE.fd $out/share/OVMF/OVMF_CODE.fd
          cp ${pkgs.OVMF.fd}/FV/OVMF_VARS.fd $out/share/OVMF/OVMF_VARS.fd
        '';

        fhs = pkgs.buildFHSEnv {
          name = "claude-desktop";
          targetPkgs =
            p:
            (with p; [
              unpacked
              glib
              gtk3
              pango
              cairo
              gdk-pixbuf
              librsvg
              atk
              at-spi2-atk
              at-spi2-core
              nss
              nspr
              cups
              dbus
              expat
              libdrm
              mesa
              libgbm
              libGL
              libxkbcommon
              vulkan-loader
              alsa-lib
              libnotify
              libsecret
              libx11
              libxcomposite
              libxdamage
              libxext
              libxfixes
              libxrandr
              libxcb
              libxtst
              libxi
              libxcursor
              libxrender
              libxscrnsaver
              libxshmfence
              libxkbfile
              udev
              libuuid
              zlib
              fontconfig
              freetype
              xdg-utils
              cacert
              pciutils
              qemu
              virtiofsd # Cowork VM
            ])
            ++ [ ovmfDebian ];
          # awesome sets XDG_CURRENT_DESKTOP=none+awesome, so Chromium's OSCrypt
          # auto-picks the "basic" (plaintext) backend and never opens a Secret
          # Service session. Force libsecret so secrets go through org.freedesktop.secrets.
          runScript = "${unpacked}/lib/claude-desktop/claude-desktop --no-sandbox --password-store=gnome-libsecret";
        };
      in
      {
        packages.default =
          pkgs.runCommand "claude-desktop-${version}"
            {
              meta = with pkgs.lib; {
                description = "Claude Desktop (official Anthropic Linux build), wrapped for NixOS";
                homepage = "https://claude.ai";
                license = licenses.unfree;
                platforms = [ "x86_64-linux" ];
                mainProgram = "claude-desktop";
              };
            }
            ''
              mkdir -p $out/bin $out/share
              ln -s ${fhs}/bin/claude-desktop $out/bin/claude-desktop
              cp -r ${unpacked}/share/applications $out/share/
              cp -r ${unpacked}/share/icons        $out/share/
            '';
      }
    );
}
