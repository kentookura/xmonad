# A flake-parts module for Haskell cabal projects.
{
  self,
  config,
  lib,
  flake-parts-lib,
  ...
}: let
  inherit
    (flake-parts-lib)
    mkSubmoduleOptions
    mkPerSystemOption
    ;
  inherit
    (lib)
    mkOption
    mkDefault
    types
    ;
  inherit
    (types)
    functionTo
    raw
    ;
in {
  options = {
    perSystem =
      mkPerSystemOption
      ({
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: {
        options.haskellProjects = mkOption {
          description = "Haskell projects";
          type = types.attrsOf (types.submodule {
            options = {
              wallpaper = mkOption {
                description = "Dekstop Wallpaper";
                type = types.path;
              };
            };
          });
        };
      });
  };
  config = {
    xsession = {
      enable = true;
      initExtra = builtins.readFile ./files/xinitrc;
    };
    home.packages = with pkgs; let
      remaps =
        writeShellScriptBin "remaps"
        (builtins.readFile ./files/remaps);
    in [
      remaps
      haskellPackages.xmonadKento
      sxhkd
      xdotool
      xcape
      xorg.xinit
      wmctrl
      eww
    ];
    home.file = {
      ".current_wallpaper.jpg" = {
        source = ./files/bg.jpg;
        target = ".current_wallpaper.jpg";
      };
      ".xinitrc" = {
        text = builtins.readFile ./files/xinitrc;
        target = ".xinitrc";
      };
    };
    programs.autorandr = {
      enable = true;
      profiles = {
        "laptop-dual" = {
          fingerprint = {
            "DP-2-1" = "00ffffffffffff0010acf44042393431211a010380331d78eaebf5a656519c26105054a54b00714f8180a9c0d1c00101010101010101023a801871382d40582c4500fd1e1100001e000000ff00563247353136384b313439420a000000fc0044454c4c205032333137480a20000000fd00384c1e5311000a2020202020200101020317b14c9005040302071601141f121365030c001000023a801871382d40582c4500fd1e1100001e011d8018711c1620582c2500fd1e1100009e011d007251d01e206e285500fd1e1100001e8c0ad08a20e02d10103e9600fd1e110000180000000000000000000000000000000000000000000000000000000000000000ed";
            "eDP-1" = "00ffffffffffff000dae8214000000001f160104951f117802b5359455539329235054000000010101010101010101010101010101011c2a405461841a303020350035ae10000018000000fe004e3134304647452d4541320a20000000fe00434d4e0a202020202020202020000000fe004e3134304647452d4541320a200060";
          };
          config = {
            eDP-1 = {
              enable = true;
              primary = true;
            };
            DP-2-1 = {
              enable = true;
              primary = false;
              rotate = "left";
            };
          };
        };
      };
    };
  };
}
