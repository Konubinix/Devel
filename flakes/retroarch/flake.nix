# [[id:1024ce11-1893-40bd-b889-658ef17daafe::retroarch-flake][retroarch-flake]]
# Tangled from roam/streaming_with_sunshine_moonlight.org -- edit it there, not here.
{
  description = "RetroArch with cores for SNES, N64 and Game Boy Color";
  inputs.nixpkgs.url = "nixpkgs";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      # snes9x is redistributable-but-unfree, so allow unfree in the flake's pkgs.
      forAll  = f: nixpkgs.lib.genAttrs systems (s:
        f (import nixpkgs { system = s; config.allowUnfree = true; }));

      # The Lite 2's lines, as the pis' moonlight gets them.
      sdlLines = ''
        05009075c82d00001251000000010000,8BitDo Lite 2,a:b1,b:b0,back:b10,dpdown:h0.4,dpleft:h0.8,dpright:h0.2,dpup:h0.1,guide:b12,leftshoulder:b6,leftstick:b13,lefttrigger:a5,leftx:a0,lefty:a1,rightshoulder:b7,rightstick:b14,righttrigger:a4,rightx:a2,righty:a3,start:b11,x:b4,y:b3,platform:Linux,
        0300cfdac82d00001251000011010000,8BitDo Lite 2,a:b1,b:b0,back:b10,dpdown:h0.4,dpleft:h0.8,dpright:h0.2,dpup:h0.1,guide:b12,leftshoulder:b6,leftstick:b13,lefttrigger:a5,leftx:a0,lefty:a1,rightshoulder:b7,rightstick:b14,righttrigger:a4,rightx:a2,righty:a3,start:b11,x:b4,y:b3,platform:Linux,
        030077557e0500000920000000026803,Nintendo Switch Pro Controller,a:b0,b:b1,back:b4,dpdown:b12,dpleft:b13,dpright:b14,dpup:b11,guide:b5,leftshoulder:b9,leftstick:b7,lefttrigger:a4,leftx:a0,lefty:a1,rightshoulder:b10,rightstick:b8,righttrigger:a5,rightx:a2,righty:a3,start:b6,x:b2,y:b3,misc1:b15,platform:Linux,
      '';
    in {
      packages = forAll (pkgs:
        let
          retroarch = pkgs.retroarch-bare.wrapper {
            cores = with pkgs.libretro; [
              snes9x        # SNES
              mupen64plus   # N64
              gambatte      # Game Boy / Game Boy Color
            ];
          };
        in {
          # Same name, same path: sunshine's retroarchBin and its pkill still match.
          default = pkgs.symlinkJoin {
            name = "retroarch-lite2";
            paths = [ retroarch ];
            nativeBuildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/retroarch \
                --set SDL_GAMECONTROLLER_USE_BUTTON_LABELS 0 \
                --set SDL_GAMECONTROLLERCONFIG ${pkgs.lib.escapeShellArg sdlLines}
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
# retroarch-flake ends here
