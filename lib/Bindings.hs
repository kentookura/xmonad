module Bindings
  ( myKeys
  , myMouseBindings
  )
where
--{{{
import Themes
import Topics
import Scratchpads
import Tree
import Search

import qualified Data.Map as M
import System.IO
import System.Exit

import XMonad hiding ( (|||) )

import qualified XMonad.Actions.FlexibleResize as Flex

import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.TreeSelect as TS
import           XMonad.Actions.CycleSelectedLayouts
import           XMonad.Actions.DwmPromote
import           XMonad.Actions.FloatKeys
import           XMonad.Actions.PhysicalScreens
import           XMonad.Actions.Submap
import           XMonad.Actions.TopicSpace
import           XMonad.Actions.WindowNavigation
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.BoringWindows
import           XMonad.Layout.Gaps
import           XMonad.Layout.Spacing
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Operations
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Workspace
import           XMonad.StackSet as W
import           XMonad.Util.EZConfig(additionalKeys)
import           XMonad.Util.NamedScratchpad
--}}}

myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $ 
  -- programs
  padKeys 
  ++
  [ ((modMask                 , xK_Return), spawnShell )
  , ((modMask .|. shiftMask   , xK_Return), spawn "alacritty")
  , ((modMask                 , xK_d),      spawn "rofi -matching fuzzy -modi combi -show combi -combi-modi run, drun -theme gruvbox-dark-hard")
  , ((modMask                 , xK_w),      spawn "qutebrowser")
  , ((modMask                 , xK_r),      spawn "alacritty -e ranger")
  , ((controlMask .|. mod1Mask, xK_k),      spawn "kbd")
  , ((controlMask .|. mod1Mask, xK_l),      spawn "lock")

  -- layout
  , ((modMask              , xK_Tab),    cycleThroughLayouts ["full", "mirrorTiled"])
  , ((modMask              , xK_f),      cycleThroughLayouts ["full", "tiled"])
  , ((modMask              , xK_t),      treeselectAction treeTheme)
  , ((modMask              , xK_b),      sendMessage ToggleStruts)

  , ((modMask .|. shiftMask, xK_h),      sendMessage (IncMasterN (-1)))
  , ((modMask .|. shiftMask, xK_l),      sendMessage (IncMasterN 1))
  , ((modMask .|. shiftMask .|. mod1Mask, xK_h), dwmpromote)
  , ((modMask .|. shiftMask .|. mod1Mask, xK_l), dwmpromote)
  , ((modMask .|. shiftMask, xK_space),  windows W.swapMaster)
  , ((modMask              , xK_j),      windows W.focusDown)
  , ((modMask              , xK_k),      windows W.focusUp)
  , ((modMask .|. shiftMask, xK_j),      windows W.swapDown)
  , ((modMask .|. shiftMask, xK_k),      windows W.swapUp)


  , ((modMask .|. mod1Mask, xK_h), (sendMessage . pullGroup) L)
  , ((modMask .|. mod1Mask, xK_l), (sendMessage . pullGroup) R)
  , ((modMask .|. mod1Mask, xK_k), (sendMessage . pullGroup) U)
  , ((modMask .|. mod1Mask, xK_j), (sendMessage . pullGroup) D)

  , ((modMask .|. mod1Mask, xK_m), withFocused (sendMessage . MergeAll))
  , ((modMask .|. mod1Mask, xK_u), withFocused (sendMessage . UnMerge))

  , ((modMask .|. mod1Mask, xK_g), onGroup W.focusUp')
  , ((modMask .|. mod1Mask, xK_l), onGroup W.focusDown')

  -- resizing
  , ((modMask              , xK_h), sendMessage Shrink)
  , ((modMask              , xK_l), sendMessage Expand)
  , ((modMask              , xK_u), sendMessage MirrorShrink)
  , ((modMask              , xK_i), sendMessage MirrorExpand)

  -- floating
  , ((modMask .|. shiftMask, xK_t    ), withFocused $ windows . W.sink)

  , ((modMask              , xK_Up   ), withFocused (keysMoveWindow (0  , -10)))
  , ((modMask              , xK_Down ), withFocused (keysMoveWindow (0  , 10)))
  , ((modMask              , xK_Right), withFocused (keysMoveWindow (10 ,  0)))
  , ((modMask              , xK_Left ), withFocused (keysMoveWindow (-10,  0)))

  , ((modMask .|. shiftMask, xK_Up   ), withFocused (keysResizeWindow (0  , 10) (0, 1)))
  , ((modMask .|. shiftMask, xK_Down ), withFocused (keysResizeWindow (0  ,-10) (0, 1)))
  , ((modMask .|. shiftMask, xK_Right), withFocused (keysResizeWindow (10 ,  0) (0, 0)))
  , ((modMask .|. shiftMask, xK_Left ), withFocused (keysResizeWindow (-10,  0) (0, 0)))

  -- topics
  , ((modMask              , xK_a),      currentTopicAction myTopicConfig)
  , ((modMask              , xK_g),      promptedGoto)
  , ((modMask .|. shiftMask, xK_g),      promptedShift)

  -- util
  , ((modMask .|. shiftMask, xK_c),      kill)  
  , ((modMask              , xK_q),      restart "xmonad" True)
  , ((modMask .|. shiftMask, xK_q),      confirmPrompt hotPromptTheme "quit XMonad" $ io exitSuccess)

  , ((shiftMask, xK_F2), spawn "pamixer -d 5")
  , ((shiftMask, xK_F2), spawn "pamixer -i 5")]
  ++
  [((m .|. modMask, key), windows $ f workspace)
    | (workspace, key) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  ++
  -- screens
  [((m .|. modMask, key), f sc)
    | (key, sc) <- zip [xK_m, xK_comma, xK_period] [0..]
    , (f, m)    <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]]
  -- search
  --[((modMask, xK_s), submap $ searchList $ S.promptSearch promptTheme)]
  -- ++
  -- [((modMask, k), S.selectSearch f) | (k,f) <- searchList ]



myMouseBindings XConfig {XMonad.modMask = modMask} = M.fromList
  [ ((modMask,               button1), \w -> XMonad.Operations.focus w >> mouseMoveWindow w )
  , ((modMask .|. shiftMask, button1), \w -> XMonad.Operations.focus w >> Flex.mouseResizeWindow w )
  --, ((modMask,               button2), \w -> XMonad.Operations.focus w >> windows W.sink)
  , ((modMask,               button4), \w -> XMonad.Operations.focus w >> windows W.focusDown)
  , ((modMask,               button5), \w -> XMonad.Operations.focus w >> windows W.focusUp)
  ]
