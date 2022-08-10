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
      packageName = "xmonadKento";
      versionNum = "0.1.0";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowBroken = true;
        };
      in rec {
        packages.${packageName} = pkgs.stdenv.mkDerivation rec {
          pname = "${packageName}";
          version = "${versionNum}";

          src = ./src;

          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.ghc
          ];

          unpackPhase =
            ''
              cp -r $src/* .
            '';

          configurePhase =
            ''
              mkdir -p cabal
              CABAL_DIR=$PWD/cabal cabal user-config init
              sed --in-place '/^repository /d; /^ *url:/d; /^ *--/d' cabal/config
            '';

          buildPhase =
            ''
              CABAL_DIR=$PWD/cabal cabal build
            '';

          installPhase =
            ''
              CABAL_DIR=$PWD/cabal cabal install
              mkdir -p $out/bin
              cp cabal/bin/${packageName} $out/bin/
            '';
        };

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
        nixosModules.${packageName} =
          { pkgs, ...}:
          {
            nixpkgs.overlays = [ self.overlay ];
            environment.systemPackages = [pkgs.${packageName} ];
          };
      });
      #// {
      #  inherit overlay overlay;
      #};
}
