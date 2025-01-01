let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.haskellPackages.parsec
  ];
  shellHook = ''
    cabal update
    cabal build
  '';
}
