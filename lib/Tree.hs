module Tree 
  ( treeselectAction
  , treeTheme)

where

import Themes
import           Data.Tree
import qualified Data.Map as M
import           XMonad
import qualified XMonad.Actions.TreeSelect as TS
import           XMonad.Hooks.WorkspaceHistory
import qualified XMonad.StackSet as W

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a =  TS.treeselectAction a
        [ Node  (TS.TSNode "+ Uni" "Current Lectures" (return ()))
             [ Node (TS.TSNode "Algebra" "" (spawn "alacritty -e ranger uni/alg")) []
             , Node (TS.TSNode "+ Logic" "" (return()))
                 [ Node (TS.TSNode "Script" "" (spawn "zathura ~/uni/log/script.pdf")) []
                 , Node (TS.TSNode "ranger" "" (spawn "alacritty -e ranger uni/log")) []
                 ]
             ]
        , Node  (TS.TSNode "+ System" "System Controls" (return ()))
             [ Node (TS.TSNode "Shutdown" "" (spawn "sudo shutdown now")) []
             , Node (TS.TSNode "Reboot"   "" (spawn "sudo reboot now")) []
             , Node (TS.TSNode "exit X"   "" (spawn "pkill xinit")) []
             ]

        , Node (TS.TSNode "+ docs" "my documents" (return()))
             [ Node (TS.TSNode "What I wish I knew when learning Haskell" "" (spawn "zathura ~/doc/haskell.pdf")) []
             , Node (TS.TSNode "Homotopy Type Theory" "" (spawn "zathura ~/uni/hott.pdf")) []
             ]

        , Node  (TS.TSNode "+ Site" "Edit my Webpage" (return ()))
          [ Node (TS.TSNode "index"     "" (spawn "alacritty --working-directory /site -e vim ~/site/src/index.html")) []
          , Node (TS.TSNode "site.hs"   "" (spawn "alacritty --working-directory ~/site/ -e vim ~/site/site.hs")) []
          , Node (TS.TSNode "templates" "" (spawn "alacritty --hold -e ranger ~/site/src/templates" )) []
          ]
        ]


treeTheme = TS.TSConfig 
  { TS.ts_hidechildren = True
  , TS.ts_background   = 0xdd282c34
  , TS.ts_font         = myFont
  , TS.ts_node         = (0xffC4C4C4, 0xff1c1f24)
  , TS.ts_nodealt      = (0xffC4C4C4, 0xff1c1f24)
  , TS.ts_highlight    = (0xffffffff, 0xff8f3f71)
  , TS.ts_extra        = 0xffd0d0d0
  , TS.ts_node_width   = 200
  , TS.ts_node_height  = 20
  , TS.ts_originX      = 100
  , TS.ts_originY      = 100
  , TS.ts_indent       = 80
  , TS.ts_navigate     = myTreeNavigation
  }

myTreeNavigation = M.fromList
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_space),    TS.select)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)
    , ((0, xK_k),        TS.movePrev)
    , ((0, xK_j),        TS.moveNext)
    , ((0, xK_h),        TS.moveParent)
    , ((0, xK_l),        TS.moveChild)
    , ((0, xK_o),        TS.moveHistBack)
    , ((0, xK_i),        TS.moveHistForward)
    ]
