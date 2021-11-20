{-
                                                                  _/
   _/    _/  _/_/_/  _/_/      _/_/    _/_/_/      _/_/_/    _/_/_/
    _/_/    _/    _/    _/  _/    _/  _/    _/  _/    _/  _/    _/
 _/    _/  _/    _/    _/  _/    _/  _/    _/  _/    _/  _/    _/
_/    _/  _/    _/    _/    _/_/    _/    _/    _/_/_/    _/_/_/
                                                                      -}
module Main where

import           Bindings
import           Data.IORef
import           Data.List                       (isPrefixOf)
import qualified Data.Map                        as M hiding (Union)
import qualified Data.Set                        as S hiding (Union)
import           Layouts
import           Log
import           Projects
import           Scratchpads
import           System.IO
import           Themes
import           XMonad
import           XMonad.Actions.DynamicProjects
import           XMonad.Actions.NoBorders
import           XMonad.Actions.TopicSpace
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.PositionStoreHooks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.ButtonDecoration
import           XMonad.Layout.DecorationAddons
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Tabbed
import           XMonad.ManageHook
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                 (spawnPipe)
import           XMonad.Util.SpawnNamedPipe

--------------------------------------------------------------------------------
--

main :: IO ()
main = do
  --checkTopicConfig myTopics myTopicConfig
  xmonad $
    ewmh $
      docks $
        dynamicProjects projects $
          desktopConfig
            { startupHook = myStartupHook
            --, layoutHook         = lessBorders OnlyScreenFloat myLayout
            , layoutHook = myLayout
            , modMask = mod4Mask
            , manageHook = myManageHook
            , handleEventHook = myHandleEventHook
            , keys = myKeys
            , mouseBindings = myMouseBindings
            , focusFollowsMouse = False
            , clickJustFocuses = False
            , XMonad.workspaces = map show [(1 :: Int) .. 12] ++ map (('W' :) . show) [(1 :: Int) .. 12]
            , logHook = myLogHook
            , borderWidth = 0
            , normalBorderColor = black
            , focusedBorderColor = purple
            }

--------------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
  spawnNamedPipe "xmobar ~/.config/nixpkgs/xmonad/xmobar/xmobar_top" "xmobar"

myNSManageHook :: ManageHook
myNSManageHook = namedScratchpadManageHook pads

myManageHook :: ManageHook
myManageHook =
  composeAll . concat $
    [ [myNSManageHook],
      [title =? "x9term" --> doFloat],
      [className =? "Msgcompose" --> doFloat],
      [className =? "zoom" <&&> title =? "Chat" --> doFloat],
      [positionStoreManageHook Nothing]
    ]

myHandleEventHook = positionStoreEventHook
