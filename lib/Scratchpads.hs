module Scratchpads
  ( pads,
    padKeys,
  )
where

import XMonad
import XMonad.ManageHook
import XMonad.Operations
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

{- ORMOLU_DISABLE -}

pads :: [NamedScratchpad]
pads = [ NS "cal"     "alacritty -t calendar -e calcurse" (title     =? "cal")        nonFloating
       , NS "htop"    "alacritty -t htop -e /bin/htop"    (title     =? "htop")       htopHook
       , NS "term"    "alacritty -t term"                 (title     =? "term")       termHook
       , NS "lf"      "alacritty -t lf -e lf"             (title     =? "lf")         mailHook
       , NS "discord" "discord"                           (className =? "discord")    discordHook
       , NS "mail"    "alacritty -t mail -e neomutt"      (title     =? "mail")       mailHook
       ]
         where htopHook    = customFloating $ rr (1/3) (1/37) (2/3) (2/3)
               discordHook = customFloating $ rr   0   (1/37) (2/3) (2/3)
               mailHook    = customFloating $ rr (1/6) (1/6)  (2/3) (2/3)
               termHook    = customFloating $ rr (1/3) (1/37) (2/3) (2/3)
               kbdHook     = customFloating $ rr (1/2)   1      0     0
               rr = W.RationalRect

padKeys =
  [ ((mod4Mask             , xK_space), namedScratchpadAction pads "term")
  , ((mod4Mask             , xK_r),     namedScratchpadAction pads "lf")
  , ((mod4Mask             , xK_c),     namedScratchpadAction pads "cal")
  , ((mod4Mask .|. mod1Mask, xK_q),     namedScratchpadAction pads "htop")
  , ((mod4Mask .|. mod1Mask, xK_d),     namedScratchpadAction pads "discord")
  ]
