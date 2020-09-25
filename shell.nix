let
  sysPkg = import <nixpkgs> { };
  releasedPkgs = sysPkg.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "20.03";
    sha256 = "12353i02hrdfa6kcla5h1q3j50mx39fchva7z7l32pk699nla4hi";
  };
  pinnedPkgs = sysPkg.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "2b8a7711f63a701d80e1b2047e4e39e00f298165";
    sha256 = "0z5m47pyc2pyv9nnvf3rrgxj77zlh8zgaif16gicnz02h6llggk3";
  };

  released_pkgs = import pinnedPkgs {};
  pinned_pkgs = import pinnedPkgs {};
  stdenv = released_pkgs.stdenv;

in stdenv.mkDerivation {
  name = "env";
  buildInputs = [ released_pkgs.gnumake
                  pinned_pkgs.erlangR23
                ];
  shellHook = ''
  '';

}
