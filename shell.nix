let
  pkgs = import <nixpkgs> {};
in
pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghc98
    pkgs.cabal-install
  ];
  shellHook = ''
    cabal update
    cabal build
  '';
}
