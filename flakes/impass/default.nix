{ pkgs }:

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
  ];

  nativeBuildInputs = [
    pkgs.gobject-introspection
    pkgs.wrapGAppsHook3
  ];

  buildInputs = [
    pkgs.gtk4
  ];

  doCheck = false;

  meta = with pkgs.lib; {
    mainProgram = "impass";
    description = "Simple and secure password management system";
    homepage = "https://salsa.debian.org/debian/impass";
    license = licenses.gpl3Plus;
  };
}
