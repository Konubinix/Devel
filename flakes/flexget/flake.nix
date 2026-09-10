{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        flexgetWithDeps = pkgs.flexget.overridePythonAttrs (old: {
          propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ [
            pkgs.python3.pkgs.requests
            pkgs.python3.pkgs.beautifulsoup4
            pkgs.python3.pkgs.lxml
            pkgs.python3.pkgs.html2text
            pkgs.python3.pkgs.python-dateutil
            pkgs.python3.pkgs.six
            pkgs.python3.pkgs.cryptography
          ];

          # `plugins` exits 0 even when imports fail, hence the grep
          doInstallCheck = true;
          postInstallCheck = ''
            export HOME=$TMPDIR
            echo 'tasks: {}' > $TMPDIR/check.yml
            $out/bin/flexget -c $TMPDIR/check.yml plugins > $TMPDIR/check.log 2>&1 || true
            if grep -q 'failed to import dependencies' $TMPDIR/check.log; then
              grep 'failed to import dependencies' $TMPDIR/check.log
              exit 1
            fi
          '';
        });
      in
      {
        packages.default = flexgetWithDeps;
      }
    );
}
