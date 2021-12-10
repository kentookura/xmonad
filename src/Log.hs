{-# LANGUAGE OverloadedStrings #-}

module Log
  ( myXmonadPrinter
  , myPolyBarWithDBus
  ) where

import           Themes

import qualified Codec.Binary.UTF8.String        as UTF8
import qualified DBus                            as D
import qualified DBus.Client                     as D
import           Data.String

import           XMonad
import           XMonad.Actions.WorkspaceNames
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import qualified XMonad.StackSet                 as W
import           XMonad.Util.ClickableWorkspaces
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.WorkspaceCompare


myPolybarLogHook dbus = dynamicLogWithPP (polybarHook dbus)

myStatusBar :: StatusBarConfig
myStatusBar = statusBarProp startupCmd $ workspaceNamesPP myXmonadPrinter

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

myXmonadPrinter :: PP
myXmonadPrinter = def
  { ppTitle   = xmobarColor white "" . shorten 150
  , ppCurrent = xmobarColor blue "" . wrap "[" "]"
  , ppLayout  = layoutMap
  , ppVisible = xmobarColor white "" . wrap "/" "/"
  , ppHidden  = id
  , ppSep     = xmobarColor purple "" " | "
  , ppExtras  = [windowCount]
  , ppSort    = (. filterOutWs [scratchpadWorkspaceTag]) <$> ppSort def
  }

myPolybarPrinter :: PP
myPolybarPrinter = def
  { ppCurrent = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
  , ppVisible = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
  , ppUrgent  = wrap ("%{F" ++ red ++ "} ") " %{F-}"
  , ppHidden  = wrap " " " "
  , ppWsSep   = " "
  , ppSep     = " | "
  , ppSort    = (. filterOutWs [scratchpadWorkspaceTag]) <$> ppSort def
  }

myPolyBarWithDBus :: D.Client -> PP
myPolyBarWithDBus dbus = myPolybarPrinter { ppOutput = dbusOutput dbus }

mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = D.signal opath iname mname
      body   = [D.toVariant $ UTF8.decodeString str]
  in  D.emit dbus $ signal { D.signalBody = body }

polybarHook :: D.Client -> PP
polybarHook dbus =
  let wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
                  | otherwise  = mempty
      blue   = "#2E9AFE"
      gray   = "#7F7F7F"
      orange = "#ea4300"
      purple = "#9058c7"
      red    = "#722222"
  in  def { ppOutput          = dbusOutput dbus
          , ppCurrent         = wrapper blue
          , ppVisible         = wrapper gray
          , ppUrgent          = wrapper orange
          , ppHidden          = wrapper gray
          , ppHiddenNoWindows = wrapper red
          , ppTitle           = wrapper purple . shorten 90
          }


layoutMap :: String -> String
layoutMap l = case l of
  "tiled"       -> "<icon=tile.xbm/>"
  "mirrorTiled" -> "<icon=bstack.xbm/>"
  "full"        -> "<icon=monocle.xbm/>"
  l             -> l
