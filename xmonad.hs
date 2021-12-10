module Main where

import           Bindings
import           Layouts
import           Log
import           Projects
import           Scratchpads
import           Themes

import           Control.Monad
import qualified DBus                            as D
import qualified DBus.Client                     as D
import           Data.Function                   (on)
import           Data.List                       (sortBy)
import qualified Data.Map                        as M hiding (keys, map)
import           Data.Monoid



import           XMonad
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.Promote
import           XMonad.Actions.WorkspaceNames
import           XMonad.Config.Desktop
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.PositionStoreHooks
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.ManageHook
import qualified XMonad.StackSet                 as W
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.NamedWindows        (getName)
import           XMonad.Util.Replace
import           XMonad.Util.Run                 (safeSpawn)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  dbus <- D.connectSession
  D.requestName
    dbus
    (D.busName_ "org.xmonad.log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad myConfig
    { logHook = workspaceNamesPP (myPolyBarWithDBus dbus) >>= dynamicLogWithPP
    }

myConfig = ewmh . workspaceNamesEwmh . docks $ def
  { normalBorderColor  = black
  , focusedBorderColor = primary
  , layoutHook         = myLayout
  , manageHook         = myManageHook
  , handleEventHook    = myHandleEventHook
  , workspaces         = map show [(1 :: Int) .. 10]
  , modMask            = mod4Mask
  , keys               = myKeys
  , mouseBindings      = myMouseBindings
  , borderWidth        = 5
  , focusFollowsMouse  = False
  , clickJustFocuses   = False
  , startupHook        = spawn "polybar -c ~/nix/polybar/config laptop"
  }

myHandleEventHook :: Event -> X All
myHandleEventHook = positionStoreEventHook <> serverModeEventHook

myNSManageHook :: ManageHook
myNSManageHook = namedScratchpadManageHook pads

myManageHook :: ManageHook
myManageHook =
  composeAll
    . concat
    $ [ [myNSManageHook]
      , [title =? "x9term" --> doFloat]
      , [className =? "Msgcompose" --> doFloat]
      , [className =? "zoom" <&&> title =? "Chat" --> doFloat]
      , [positionStoreManageHook Nothing]
      ]


-- Unused
--------------------------------------------------------------------------------

myDirs = Directories { cfgDir   = "~/xmonad"
                     , dataDir  = "~/xmonad/data"
                     , cacheDir = "~/xmonad/cachd"
                     }

myStartupHook :: X ()
myStartupHook = do
  mapM_
    (spawn . (++ " &"))
    [ "xset r rate 300 50"
    , "setxkbmap -option caps:super"
    , "killall xcape 2>/dev/null ; xcape -e 'Super_L=Escape'"
    , "hashwall -f '#282828' -b '#1e1b1c' -s 12"
    , "unclutter &"
    , "dunst"
    , "onboard"
    , "bin/eww daemon"
    , "termonad"
    ]

raiseHook :: X ()
raiseHook = do
  ws <- gets windowset
  withFocused $ \w -> do
    when (isFloat w ws) (focus w >> promote)
  where isFloat w ss = M.member w $ W.floating ss
