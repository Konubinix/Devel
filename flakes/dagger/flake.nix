{
  description = "dagger env with Python SDK";

  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    dagger.url = "github:dagger/nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      dagger,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        daggerCli = dagger.packages.${system}.dagger;
        python = pkgs.python3.withPackages (ps: [
          ps.anyio
          ps.pip
          ps.virtualenv
        ]);
        dagger-python = pkgs.writeShellScriptBin "dagger-python" ''
          VENV_DIR="''${XDG_CACHE_HOME:-$HOME/.cache}/dagger-venv"
          if [ ! -d "$VENV_DIR" ]; then
            ${python}/bin/python3 -m venv --system-site-packages "$VENV_DIR"
            "$VENV_DIR/bin/pip" install --quiet dagger-io opentelemetry-exporter-otlp-proto-grpc
          fi
          export PATH="${daggerCli}/bin:$PATH"
          export LD_LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib}/lib:''${LD_LIBRARY_PATH:-}"
          exec "$VENV_DIR/bin/python3" "$@"
        '';
        deps = [
          daggerCli
          python
          dagger-python
        ];
      in
      {
        packages.default = pkgs.buildEnv {
          name = "dagger";
          paths = deps;
        };
        devShell = pkgs.mkShell {
          buildInputs = deps;
          shellHook = ''
            VENV_DIR="''${XDG_CACHE_HOME:-$HOME/.cache}/dagger-venv"
            if [ ! -d "$VENV_DIR" ]; then
              python3 -m venv --system-site-packages "$VENV_DIR"
              "$VENV_DIR/bin/pip" install --quiet dagger-io opentelemetry-exporter-otlp-proto-grpc
            fi
            export PATH="$VENV_DIR/bin:$PATH"
          '';
        };
      }
    );
}
