{-
                                                                  _/
   _/    _/  _/_/_/  _/_/      _/_/    _/_/_/      _/_/_/    _/_/_/
    _/_/    _/    _/    _/  _/    _/  _/    _/  _/    _/  _/    _/
 _/    _/  _/    _/    _/  _/    _/  _/    _/  _/    _/  _/    _/
_/    _/  _/    _/    _/    _/_/    _/    _/    _/_/_/    _/_/_/
                                                                      -}
module Main where

import           Bindings
import           Layouts
import           Log
import           Projects
import           Scratchpads
import           Themes

import           Data.IORef
import           Data.List                       (isPrefixOf)
import qualified Data.Map                        as M hiding (Union)
import           Data.Monoid
import qualified Data.Set                        as S hiding (Union)
import           System.IO

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
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.StatusBar
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
  xmonad
    . withSB myStatusBar
    . ewmh
    . docks
    . dynamicProjects projects
    $ desktopConfig
        { layoutHook         = lessBorders (Combine Difference Screen OnlyFloat)
                                           myLayout
       -- (Combine Difference Screen OnlyFloat)
        , modMask            = mod4Mask
        , manageHook         = myManageHook
        , handleEventHook    = myHandleEventHook
        , keys               = myKeys
        , mouseBindings      = myMouseBindings
        , focusFollowsMouse  = False
        , clickJustFocuses   = False
        , borderWidth        = 2
        , normalBorderColor  = black
        , focusedBorderColor = primary
        , workspaces         = map show [(1 :: Int) .. 12]
                                 ++ map (('W' :) . show) [(1 :: Int) .. 12]
        --, logHook            = myLogHook
        --, startupHook        = myStartupHook
        --, layoutHook         = myLayout
        }

--------------------------------------------------------------------------------

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
      , [title =? "term" --> hasBorder True]
      ]

myHandleEventHook :: Event -> X All
myHandleEventHook = positionStoreEventHook <> serverModeEventHook
