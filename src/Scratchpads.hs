module Scratchpads
  ( pads
  , padKeys
  ) where

import           XMonad
import           XMonad.ManageHook
import           XMonad.Operations
import qualified XMonad.StackSet             as W
import           XMonad.Util.NamedScratchpad

pads :: [NamedScratchpad]
pads =
  [ NS "term" "alacritty -t term" (title =? "term") termHook
  , NS "lf" "alacritty -t lf -e lf" (title =? "lf") lfHook
  ]
 where
  lfHook    = customFloating $ rr (1 / 6) (1 / 6) (2 / 3) (2 / 3)
  termHook    = customFloating $ rr (1 / 3) (1 / 37) (2 / 3) (2 / 3)
  rr          = W.RationalRect

padKeys =
  [ ((mod4Mask, xK_space)         , namedScratchpadAction pads "term")
  , ((mod4Mask, xK_r)             , namedScratchpadAction pads "lf")
  ]
