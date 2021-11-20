module Layouts
  ( myLayout,
  )
where

import qualified Data.Map as M
import Deco
import Themes (barTheme, decoTheme, tabTheme)
import XMonad hiding ((|||))
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BorderResize
import XMonad.Layout.BoringWindows hiding (Replace)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.TrackFloating
import XMonad.Layout.WindowArranger
import XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet as W

data EmptyShrinker = EmptyShrinker
  deriving (Read, Show)

instance Shrinker EmptyShrinker where
  shrinkIt _ _ = []

--deco = myDeco EmptyShrinker decoTheme
deco = noFrillsDeco shrinkText barTheme

myLayout =
  boringWindows . trackFloating $
    tiled ||| mirrorTiled ||| full ||| floating
  where
    tiled =
      renamed [Replace "tiled"]
        . avoidStruts
        . deco
        . addTabs shrinkText tabTheme
        . windowNavigation
        . subLayout [] Simplest
        $ ResizableTall 1 (1 / 50) (1 / 2) []
    mirrorTiled =
      renamed [Replace "mirrorTiled"]
        . avoidStruts
        . deco
        . addTabs shrinkText tabTheme
        . windowNavigation
        . subLayout [] Simplest
        $ Mirror $ ResizableTall 1 (2 / 100) (1 / 2) []
    full =
      renamed [Replace "full"]
        . avoidStruts
        $ noBorders Full
    floating =
      renamed [Replace "floating"]
        . deco
        . borderResize
        $ positionStoreFloat
