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
  , dark
  , white
  , blue
  ) where

import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Prompt
import           XMonad.Prompt.Workspace
import           XMonad.Util.Image

myFont = "xft:Dina:bold:size=10:antialias=true"
black = "#32302f"
green = "#828e6f"
red = "#cc241d"
purple = "#8f3f71"
dark = "#665c54"
white = "#C4C4C4"
blue = "#3579A8"

primary = purple

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
