module Main where
import           Bindings
import           Layouts
import           Log
import           Projects
import           Scratchpads
import           Themes

import           Control.Monad
import qualified Data.Map                        as M hiding (keys, map)
import           Data.Monoid

import           XMonad
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.Promote
import           XMonad.Config.Desktop
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.PositionStoreHooks
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.StatusBar
import           XMonad.ManageHook
import qualified XMonad.StackSet                 as W
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Replace

--------------------------------------------------------------------------------

main :: IO ()
main = do
  replace
  launch myConfig myDirs

myConfig = withSB myStatusBar . ewmh . docks . dynamicProjects projects $ def
  { normalBorderColor  = black
  , focusedBorderColor = primary
  , terminal           = "termonad"
  , layoutHook         = myLayout
  , manageHook         = myManageHook
  , handleEventHook    = myHandleEventHook
  , workspaces         = map show [(1 :: Int) .. 10]
  , modMask            = mod4Mask
  , keys               = myKeys
  , mouseBindings      = myMouseBindings
  , borderWidth        = 5
  --, logHook            = raiseHook
  , startupHook        = mempty
  , focusFollowsMouse  = False
  , clickJustFocuses   = False
  }

myDirs = Directories { cfgDir   = "~/xmonad"
                     , dataDir  = "~/xmonad/data"
                     , cacheDir = "~/xmonad/cachd"
                     }

--------------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
  mapM_
    (spawn . ("exec " ++))
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


raiseHook :: X ()
raiseHook = do
  ws <- gets windowset
  withFocused $ \w -> do
    when (isFloat w ws) (focus w >> promote)
  where isFloat w ss = M.member w $ W.floating ss
