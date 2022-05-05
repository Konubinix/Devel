{ lib, buildGoModule, fetchFromGitHub }:
buildGoModule rec {
  pname = "kubecolor";
  version = "0.0.20";

  vendorSha256 = "sha256-C1K7iEugA4HBLthcOI7EZ6H4YHW6el8X6FjVN1BeJR0=";

  src = fetchFromGitHub {
    owner = "hidetatz";
    repo = "kubecolor";
    rev = "v${version}";
    sha256 = "sha256-bKHEp9AxH1CcObhNzD3BkNOdyWZu7JrEdsXpo49wEcI=";
  };

  meta = with lib; {
    description = "Colorize your kubectl output";
    homepage = "https://github.com/hidetatz/kubecolor";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
