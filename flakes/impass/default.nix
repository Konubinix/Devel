{ pkgs }:

let
  python-xdo = pkgs.python3.pkgs.buildPythonPackage rec {
    pname = "xdo";
    version = "0.5";
    src = pkgs.python3.pkgs.fetchPypi {
      inherit pname version;
      hash = "sha256-cEJG7CqGL5ooQ7uALed7vE9ja/VdUifN0qrZ3Ez8jU4=";
    };
    pyproject = true;
    build-system = [ pkgs.python3.pkgs.setuptools ];
    propagatedBuildInputs = [ pkgs.xdotool ];
    # Patch ctypes to find libxdo.so from xdotool
    postPatch = ''
      substituteInPlace xdo/_xdo.py \
        --replace-fail "find_library(\"xdo\")" '"${pkgs.xdotool}/lib/libxdo.so"'
    '';
    doCheck = false;
  };
in

pkgs.python3.pkgs.buildPythonApplication rec {
  pname = "impass";
  version = "0.14.1";
  format = "setuptools";

  src = pkgs.fetchgit {
    url = "https://salsa.debian.org/debian/impass.git";
    rev = "refs/tags/${version}";
    sha256 = "sha256-oZHj+0uJNFHReaA9CGlnEQl8WTgUG8TjQU44Skkgl0c=";
  };

  propagatedBuildInputs = with pkgs.python3.pkgs; [
    gpgme
    pygobject3
    python-xdo
  ];

  nativeBuildInputs = [
    pkgs.gobject-introspection
    pkgs.wrapGAppsHook4
  ];

  buildInputs = [
    pkgs.gtk4
  ];

  doCheck = false;

  postInstall = ''
    cp ${./konix_impass.py} $out/bin/konix_impass.py
    chmod +x $out/bin/konix_impass.py
    patchShebangs $out/bin/konix_impass.py
  '';

  meta = with pkgs.lib; {
    mainProgram = "impass";
    description = "Simple and secure password management system";
    homepage = "https://salsa.debian.org/debian/impass";
    license = licenses.gpl3Plus;
  };
}
