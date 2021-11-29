{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    xmonad-contrib = {
      url = "git+https://github.com/xmonad/xmonad-contrib";
    };
    xmonad = {
      url = "git+https://github.com/xmonad/xmonad";
    };
  };
  outputs = { self, flake-utils, nixpkgs, xmonad, xmonad-contrib }:
  let
    overlay = import ./overlay.nix;
    overlays = [ overlay xmonad.overlay xmonad-contrib.overlay ];
  in flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs {
      inherit system overlays;
      config.allowBroken = true;
    };
  in rec {
    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [ p.my-xmonad p.xmonad-contrib ];
      buildInputs = with pkgs.haskellPackages; [
        cabal-install
        haskell-language-server
        hlint
        ghcid
        ormolu
        implicit-hie
      ];
    };
    defaultPackage = pkgs.haskellPackages.my-xmonad;
  }) // {
    inherit overlay overlays;
  };
}
