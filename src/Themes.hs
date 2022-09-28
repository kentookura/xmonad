module Themes
  ( barTheme
  , primary
  , dirTheme
  , tabTheme
  , promptTheme
  , hotPromptTheme
  , myFont
  , black
  , purple
  , red
  , dark
  , white
  , blue
  ) where

import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Prompt
import           XMonad.Prompt.Workspace
import           XMonad.Util.Image
import           Data.Colour.SRGB

myFont = "xft:Scientifica:bold:size=12:antialias=true"
black = "#2b3339"
green = "#a7c080"
red = "#cc241d"
purple = "#8f3f71"
dark = "#665c54"
white = "#C4C4C4"
blue = "#3579A8"
bg0 = "#2f383e"
bg1 = "#323c41"
bg2 = "#3a454a"
bg3 = "#445055"
bg4 = "#4c555b"
bg5 = "#53605c"
bg_visual = "#503946"
bg_red = "#4e3e43"
bg_blue = "#394f5a"
bg_yellow = "#4a4940"

accent = "#404D44"
shade_1 = "#809085"
shade_2 = "#5E6E63"
shade_3 = "#1C241E"
shade_4 = "#14271A"


primary = shade_1
bg = bg0

barTheme = def { fontName            = myFont
               , inactiveBorderColor = black
               , inactiveColor       = black
               , inactiveTextColor   = black
               , activeBorderColor   = primary
               , activeColor         = primary
               , activeTextColor     = primary
               , urgentTextColor     = primary
               , urgentBorderColor   = primary
               , decoHeight          = 7
               }

tabTheme = def { fontName            = myFont
               , inactiveBorderColor = black
               , inactiveColor       = black
               , inactiveTextColor   = dark
               , activeBorderColor   = primary
               , activeColor         = primary
               , activeTextColor     = black
               , urgentTextColor     = primary
               , urgentBorderColor   = primary
               }

promptTheme = def { font              = myFont
                  , bgColor           = black
                  , fgColor           = white
                  , bgHLight          = dark
                  , fgHLight          = "#1d2021"
                  , borderColor       = "#3c3836"
                  , promptBorderWidth = 2
                  , position          = Bottom
                  , height            = 25
                  }

dirTheme = def { font         = myFont
               , bgColor      = black
               , fgColor      = white
               , bgHLight     = dark
               , position     = CenteredAt (1 / 2) (1 / 2)
               , maxComplRows = Just 10
               }

hotPromptTheme = def { font              = myFont
                     , bgColor           = black
                     , fgColor           = red
                     , bgHLight          = dark
                     , fgHLight          = "#1d2021"
                     , borderColor       = "#3c3836"
                     , promptBorderWidth = 2
                     , position          = Top
                     , height            = 25
                     }
