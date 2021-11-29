module Themes
  ( barTheme
  , dirTheme
  , decoTheme
  , tabTheme
  , promptTheme
  , hotPromptTheme
  , myFont
  , black
  , purple
  , dark
  , white
  , blue
  )

where

import           Deco
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Prompt
import           XMonad.Prompt.Workspace
import           XMonad.Util.Image

myFont = "xft:Dina:bold:size=10:antialias=true"
black  = "#282828"
green  = "#828e6f"
red    = "#cc241d"
purple = "#8f3f71"
dark   = "#665c54"
white  = "#C4C4C4"
blue   = "#3579A8"

primary = green

decoTheme =
  def
    { activeColor = primary
    , activeTextColor = black
    , inactiveTextColor = primary
    , inactiveColor = black
    , activeBorderWidth = 0
    , inactiveBorderWidth = 0
    , fontName = myFont
    , decoHeight = 15
    , windowTitleIcons = [
      (lambdaSymbol, CenterLeft 5),
      (closeButton, CenterRight 3),
      (maxiButton, CenterRight 18),
      (miniButton, CenterRight 33)]
    }

barTheme =
    def
      { fontName            = font
      , inactiveBorderColor = black
      , inactiveColor       = black
      , inactiveTextColor   = black
      , activeBorderColor   = primary
      , activeColor         = primary
      , activeTextColor     = primary
      , urgentTextColor     = primary
      , urgentBorderColor   = primary
      , decoHeight          = decorationHeight
      }
    where
      decorationHeight = 7
      font = myFont

tabTheme =
    def
      { fontName            = font
      , inactiveBorderColor = black
      , inactiveColor       = black
      , inactiveTextColor   = dark
      , activeBorderColor   = primary
      , activeColor         = primary
      , activeTextColor     = black
      , urgentTextColor     = primary
      , urgentBorderColor   = primary
      }
    where
      font = myFont

promptTheme =
  def
    { font              = myFont
    , bgColor           = black
    , fgColor           = white
    , bgHLight          = dark
    , fgHLight          = "#1d2021"
    , borderColor       = "#3c3836"
    , promptBorderWidth = 2
    , position          = Bottom
    , height            = 25
    }

dirTheme =
  def
    { font = myFont
    , bgColor = black
    , fgColor = white
    , bgHLight = dark
    , position = CenteredAt (1/2) (1/2)
    , maxComplRows = Just 10
    }

hotPromptTheme =
  def
    { font              = myFont
    , bgColor           = black
    , fgColor           = red
    , bgHLight          = dark
    , fgHLight          = "#1d2021"
    , borderColor       = "#3c3836"
    , promptBorderWidth = 2
    , position          = Top
    , height            = 25
    }
