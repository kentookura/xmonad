{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  packages = [
    pkgs.pkg-config
    pkgs.xorg.libX11
    pkgs.xorg.xrandr
    pkgs.xorg.libXrandr
    pkgs.xorg.libXinerama
    pkgs.xorg.libXScrnSaver
    pkgs.xorg.libXext
    pkgs.xorg.libXft
    pkgs.xorg.libXfont
    pkgs.xorg.libXrender
  ];
}
