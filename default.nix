with import <nixpkgs> { };
haskellPackages.extend (haskell.lib.packageSoruceOverrides {
  xmonad = ../xmonad;
  xmonad-contrib = ../xmonad-contrib;
})
