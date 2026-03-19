{
  inputs = {
    nixpkgs.url = "nixpkgs";
    # Real SDL2 (not sdl2-compat/SDL3 shim) for scrcpy: the compat layer has a
    # YUV texture rendering bug causing a black screen on AMD GPUs.
    # TODO: remove once scrcpy 4.0 ships with native SDL3 support (merged in
    # scrcpy dev branch, PR #6216).
    nixpkgs-sdl2.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-sdl2,
      flake-utils,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        pkgs-sdl2 = import nixpkgs-sdl2 { inherit system; };
      in
      {
        packages.default = pkgs.scrcpy.override {
          SDL2 = pkgs-sdl2.SDL2;
        };
      }
    );
}
