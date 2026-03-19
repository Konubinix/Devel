{
  description = "Blender MCP - Model Context Protocol server for Blender";

  inputs.nixpkgs.url = "nixpkgs";

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {
      packages.x86_64-linux.default = pkgs.python3Packages.buildPythonApplication {
        pname = "blender-mcp";
        version = "1.5.5";
        pyproject = true;

        src = pkgs.fetchFromGitHub {
          owner = "ahujasid";
          repo = "blender-mcp";
          rev = "7636d13bded82eca58eb93c3f4cd8708dfdfbe8b";
          hash = "sha256-VGilcq/ZuX5ancdUqQpc6z7LGoBpyCMIasaTIzmTbRM=";
        };

        build-system = [
          pkgs.python3Packages.setuptools
          pkgs.python3Packages.wheel
        ];

        dependencies = with pkgs.python3Packages; [
          mcp
          supabase
          tomli
        ];

        doCheck = false;

        postInstall = ''
          mkdir -p $out/share/blender/addons $out/share/blender/startup
          cp $src/addon.py $out/share/blender/addons/blender_mcp.py
          cat > $out/share/blender/startup/enable_blender_mcp.py <<'STARTUP'
import addon_utils
addon_utils.enable("blender_mcp", default_set=True)
STARTUP
        '';
      };
    };
}
