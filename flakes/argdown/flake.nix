{
  description = "Argdown CLI (SVG/DOT/PDF, no Puppeteer)";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAll  = f: nixpkgs.lib.genAttrs systems (s: f nixpkgs.legacyPackages.${s});
    in {
      packages = forAll (pkgs: {
        default = pkgs.buildNpmPackage {
          pname = "argdown-cli";
          version = "2.0.0";
          src = ./.;

          # First build: leave fakeHash, copy the printed `got:` hash here.
          npmDepsHash = "sha256-npIaF9t8ySovuhwYYumNb0mS9qdhjkq9AdMDZ6rROnk=";

          dontNpmBuild = true;
          npmFlags = [ "--ignore-scripts" ];
          nativeBuildInputs = [ pkgs.makeWrapper ];

          installPhase = ''
            runHook preInstall
            mkdir -p $out/lib $out/bin
            cp -r node_modules $out/lib/node_modules
            makeWrapper $out/lib/node_modules/.bin/argdown $out/bin/argdown \
              --prefix PATH : ${pkgs.nodejs}/bin
            runHook postInstall
          '';
        };
      });

      devShells = forAll (pkgs: {
        default = pkgs.mkShell {
          packages = [ self.packages.${pkgs.system}.default ];
        };
      });
    };
}
