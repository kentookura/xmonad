module Themes 
  ( barTheme
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

import           XMonad.Prompt
import           XMonad.Prompt.Workspace
import           XMonad.Layout.NoFrillsDecoration

myFont = "xft:Dina:bold:size=10:antialias=true" 
black  = "#282828"
red    = "#cc241d"
purple = "#8f3f71"
dark   = "#665c54" 
white  = "#C4C4C4"
blue   = "#3579A8" 

barTheme =
    def
      { fontName            = font
      , inactiveBorderColor = black
      , inactiveColor       = black
      , inactiveTextColor   = black
      , activeBorderColor   = purple
      , activeColor         = purple
      , activeTextColor     = purple
      , urgentTextColor     = purple
      , urgentBorderColor   = purple
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
      , activeBorderColor   = purple
      , activeColor         = purple
      , activeTextColor     = black
      , urgentTextColor     = purple
      , urgentBorderColor   = purple
      }
    where
      font = myFont

promptTheme =
  def
    { font              = myFont
    , bgColor           = black
    , fgColor           = purple
    , bgHLight          = dark
    , fgHLight          = "#1d2021"
    , borderColor       = "#3c3836"
    , promptBorderWidth = 2
    , position          = Bottom
    , height            = 25
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
