{
  description = "RetroArch with cores for SNES, N64 and Game Boy Color";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      # snes9x is redistributable-but-unfree, so allow unfree in the flake's pkgs.
      forAll  = f: nixpkgs.lib.genAttrs systems (s:
        f (import nixpkgs { system = s; config.allowUnfree = true; }));
    in {
      packages = forAll (pkgs: {
        default = pkgs.retroarch.withCores (cores: with cores; [
          snes9x        # SNES
          mupen64plus   # N64
          gambatte      # Game Boy / Game Boy Color
        ]);
      });

      devShells = forAll (pkgs: {
        default = pkgs.mkShell {
          packages = [ self.packages.${pkgs.system}.default ];
        };
      });
    };
}
