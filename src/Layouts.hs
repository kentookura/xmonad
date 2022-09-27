module Layouts
  ( myLayout
  ) where

import qualified Data.Map                         as M
import           Themes                           (barTheme, tabTheme)
import           XMonad                           hiding ((|||))
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.BorderResize
import           XMonad.Layout.BoringWindows      hiding (Replace)
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.NoBorders
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.PositionStoreFloat
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.StackTile
import           XMonad.Layout.Simplest
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TrackFloating
import           XMonad.Layout.TwoPanePersistent
import           XMonad.Layout.WindowNavigation
import qualified XMonad.StackSet                  as W

deco = noFrillsDeco shrinkText barTheme

myLayout =
  boringWindows
    .   lessBorders OnlyTiled
    .   trackFloating
    $   tiled
    ||| mirrorTiled
    ||| stacked
    ||| twoPane
    ||| floating
    ||| full
 where
  twoPane =
    renamed [Replace "panes"]
      . avoidStruts
      . deco
      . addTabs shrinkText tabTheme
      . windowNavigation
      . subLayout [] Simplest
      $ TwoPanePersistent Nothing (1 / 50) (1 / 2)
  tiled =
    renamed [Replace "tiled"]
      . avoidStruts
      . deco
      . addTabs shrinkText tabTheme
      . windowNavigation
      . subLayout [] Simplest
      $ ResizableTall 1 (1 / 50) (1 / 2) []
  stacked = 
    renamed [Replace "stackTile"]
      . avoidStruts
      . deco
      . addTabs shrinkText tabTheme
      . windowNavigation
      . subLayout [] Simplest
      $ StackTile 1 (3/100) (1/2)
  mirrorTiled =
    renamed [Replace "mirrorTiled"]
      . avoidStruts
      . deco
      . addTabs shrinkText tabTheme
      . windowNavigation
      . subLayout [] Simplest
      $ Mirror
      $ ResizableTall 1 (2 / 100) (1 / 2) []
  full     = avoidStruts . renamed [Replace "full"] $ noBorders Full
  floating = renamed [Replace "floating"] . deco . borderResize $ simpleFloat

data OnlyTiled = OnlyTiled
  deriving (Read, Show)

instance SetsAmbiguous OnlyTiled where
  hiddens _ _ _ mst wrs = filter (`elem` W.integrate' mst) $ map fst wrs
