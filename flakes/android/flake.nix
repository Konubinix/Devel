{
  description = "Android dev env";
  # got from https://github.com/NixOS/nixpkgs/issues/154898

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.android_sdk.accept_license = true;
          config.allowUnfree = true;
        };
        # system-images;android-${platformVersion};${systemImageType};${abiVersion}
        android = {
          versions = {
            # tools = "26.1.1";
            # platformTools = "34.0.5";
            # buildTools = "30.0.2";
            # emulator = "34.1.9";
          };
          platforms = [ # "30"
            "34"
          ];
          abis = [ # "armeabi-v7a" "arm64-v8a"
            "x86_64"
          ];
          extras = [ # "extras;google;gcm"
          ];
        };
        androidEnv = pkgs.androidenv;
        androidComposition = androidEnv.composeAndroidPackages {
          # toolsVersion = "26.1.1";
          # platformToolsVersion = "34.0.5";
          # buildToolsVersions = [ android.versions.buildTools ];
          platformVersions = android.platforms;
          abiVersions = android.abis;

          # includeSources = true;
          includeSystemImages = true;
          includeEmulator = true;
          # emulatorVersion = android.versions.emulator;

          useGoogleAPIs = true;
          includeExtras = android.extras;
          systemImageTypes = [ "default" "google_apis" ];
          # Accepting more licenses declaratively:
          # extraLicenses = [
          #   "android-sdk-preview-license"
          #   "android-googletv-license"
          #   "android-sdk-arm-dbt-license"
          #   "google-gdk-license"
          #   "intel-android-extra-license"
          #   "intel-android-sysimage-license"
          #   "mips-android-sysimage-license"
          # ];
        };
        androidSdk = androidComposition.androidsdk;
        platformTools = androidComposition.platform-tools;
        # jdk11 is hardcoded in /nix/store/mj0hy52z22q5gpsf33akndxiclxd8ray-source/pkgs/development/mobile/androidenv/cmdline-tools.nix
        # but needs jdk17
        jdk = pkgs.jdk17;
        android_runner = pkgs.stdenv.mkDerivation rec {
          name = "android_runner";
          version = "0.1.0";

          buildInputs = [ jdk androidSdk ];

          ANDROID_SDK_ROOT = "${androidSdk}/libexec/android-sdk";

          buildPhase = ''
            #!/bin/sh
            mkdir -p $out/bin

            cat<<EOF > $out/bin/${name}
            #!/bin/sh
            # Note: ANDROID_HOME is deprecated. Use ANDROID_SDK_ROOT.
            export ANDROID_SDK_ROOT="${ANDROID_SDK_ROOT}"
            export ANDROID_NDK_ROOT="${ANDROID_SDK_ROOT}/ndk-bundle"
            export JAVA_HOME="${jdk}"
            exec "DOLLAR{@}"
            EOF
            sed -i 's/DOLLAR/$/' $out/bin/${name}
            chmod +x $out/bin/${name}
          '';

          phases = [ "buildPhase" "installPhase" ];

          meta = with pkgs.lib; {
            description = "JDK runner in the appropriate JDK";
            license = licenses.mit;
          };
        };
        deps = [
          androidSdk
          platformTools
          android_runner
          # pkgs.android-studio
          # pkgs.flutter
        ];
      in {
        packages.default = pkgs.buildEnv {
          name = "android";
          paths = with pkgs; deps;
        };
        packages.emulate = androidEnv.emulateApp {
          name = "emulate-MyAndroidApp";
          platformVersion = "28";
          abiVersion = "x86_64"; # armeabi-v7a, mips, x86_64
          systemImageType = "google_apis_playstore";
        };
        devShell = pkgs.mkShell rec {
          packages = deps;
          LANG = "C.UTF-8";
          LC_ALL = "C.UTF-8";

          # Note: ANDROID_HOME is deprecated. Use ANDROID_SDK_ROOT.
          ANDROID_SDK_ROOT = "${androidSdk}/libexec/android-sdk";
          ANDROID_NDK_ROOT = "${ANDROID_SDK_ROOT}/ndk-bundle";

          # see https://github.com/NixOS/nixpkgs/issues/154898
          # I don't need this anymore?
          # QT_QPA_PLATFORM = "xcb";
        };
      });
}
