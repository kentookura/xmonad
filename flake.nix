{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    xmonad-contrib = { url = "github:xmonad/xmonad-contrib"; };
    xmonad = { url = "github:xmonad/xmonad"; };
  };
  outputs = { self, flake-utils, nixpkgs, xmonad, xmonad-contrib }:
    let
      overlay = import ./overlay.nix;
      overlays = [ overlay xmonad.overlay xmonad-contrib.overlay ];
      nixosModule = {config, pkgs, lib, ...}:
        with lib;
        with attrsets;
        let cfg = config.graphical.xmonad;
        in {
          options = {
          };
        };
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.xmonad p.xmonad-contrib ];
          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
            hlint
            ghcid
            ormolu
            implicit-hie
          ];
        };
        defaultPackage = pkgs.haskellPackages.xmonad;
      });
      #// {
      #  inherit overlay overlay;
      #};
}
