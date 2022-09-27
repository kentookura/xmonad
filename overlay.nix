_: pkgs: rec {
  haskellPackages = pkgs.haskellPackages.override (old: {
    overrides =
      pkgs.lib.composeExtensions (old.overrides or (_: _: {}))
      (self: super: rec {
        xmonadKento = self.callCabal2nix "xmonad" ./. {};
        #my-xmonad = self.callCabal2nix "my-xmonad"
        #  (pkgs.lib.sourceByRegex ./. [ "lib/" "my-xmonad.cabal" ]) { };
      });
  });
}
