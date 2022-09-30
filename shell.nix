{ 
  pkgs ? import <nixpkgs> {};
}:
 pkgs.mkShell {
  buildInputs = with pkgs.haskellPackages; [
    cabal-install
    haskell-language-server
    hlint
    ghcid
    ormolu
    implicit-hie
  ];
}
