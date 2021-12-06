module Log
  ( myStatusBar
  ) where

import           Themes

import           XMonad
import           XMonad.Actions.WorkspaceNames
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import qualified XMonad.StackSet               as W
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.SpawnNamedPipe
import           XMonad.Util.WorkspaceCompare

myStatusBar :: StatusBarConfig
myStatusBar = statusBarProp startupCmd (pure myPrettyPrinter)

startupCmd = "xmobar ~/xmonad/xmobar/xmobar_top"

windowCount :: X (Maybe String)
windowCount =
  gets
    $ Just
    . wrap "[ " " ]"
    . show
    . length
    . W.integrate'
    . W.stack
    . W.workspace
    . W.current
    . windowset

myPrettyPrinter :: PP
myPrettyPrinter = def
  { ppTitle   = xmobarColor white "" . shorten 150
  , ppCurrent = xmobarColor blue "" . wrap "[" "]"
  , ppLayout  = layoutMap
  , ppVisible = xmobarColor white "" . wrap "/" "/"
  , ppHidden  = id
  , ppSep     = xmobarColor purple "" " | "
  , ppExtras  = [windowCount]
  , ppSort    = (. filterOutWs [scratchpadWorkspaceTag]) <$> ppSort def
  }

layoutMap :: String -> String
layoutMap l = case l of
  "tiled"       -> "<icon=tile.xbm/>"
  "mirrorTiled" -> "<icon=bstack.xbm/>"
  "full"        -> "<icon=monocle.xbm/>"
  l             -> l
