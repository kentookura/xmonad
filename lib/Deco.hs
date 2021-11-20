{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Deco
  ( myDeco,
    menuButton,
    miniButton,
    maxiButton,
    closeButton,
    lambdaSymbol,
  )
where

import           Utils
import           XMonad
import           XMonad.Actions.GridSelect
import           XMonad.Actions.Minimize
import           XMonad.Layout.Decoration
import           XMonad.Layout.DecorationAddons
import           XMonad.Layout.Maximize
import qualified XMonad.StackSet                as W
import           XMonad.Util.Image
import           XMonad.Util.XUtils             (fi)

buttonSize :: Int
buttonSize = 20

menuButtonOffset :: Int
menuButtonOffset = 4

minimizeButtonOffset :: Int
minimizeButtonOffset = 32

maximizeButtonOffset :: Int
maximizeButtonOffset = 18

closeButtonOffset :: Int
closeButtonOffset = 4

-- | A function intended to be plugged into the 'decorationCatchClicksHook' of a decoration.
-- It will intercept clicks on the buttons of the decoration and invoke the associated action.
-- To actually see the buttons, you will need to use a theme that includes them.
-- See 'defaultThemeWithImageButtons' below.
imageTitleBarButtonHandler :: Window -> Int -> Int -> X Bool
imageTitleBarButtonHandler mainw distFromLeft distFromRight = do
  let action
        | fi distFromLeft <= menuButtonOffset + buttonSize =
          spawn toggleKbd
            >> return True
        | fi distFromRight >= closeButtonOffset
            && fi distFromRight <= closeButtonOffset + buttonSize =
          focus mainw >> kill >> return True
        | fi distFromRight >= maximizeButtonOffset
            && fi distFromRight <= maximizeButtonOffset + buttonSize =
          focus mainw >> sendMessage (maximizeRestore mainw) >> return True
        | fi distFromRight >= minimizeButtonOffset
            && fi distFromRight <= minimizeButtonOffset + buttonSize =
          focus mainw >> minimizeWindow mainw >> return True
        | otherwise = focus mainw >> windowMenu >> return True >> return True
  action

toggleKbd :: String
toggleKbd = "dbus-send --type=method_call --dest=org.onboard.Onboard /org/onboard/Onboard/Keyboard org.onboard.Onboard.Keyboard.ToggleVisible"

myDeco ::
  (Eq a, Shrinker s) =>
  s ->
  Theme ->
  l a ->
  ModifiedLayout (Decoration ImageButtonDecoration s) l a
myDeco s c = decoration s c $ NFD True

newtype ImageButtonDecoration a = NFD Bool deriving (Show, Read)

instance Eq a => DecorationStyle ImageButtonDecoration a where
  describeDeco _ = "ImageButtonDeco"
  decorationCatchClicksHook _ mainw dFL dFR = imageTitleBarButtonHandler mainw dFL dFR
  decorationAfterDraggingHook _ (mainw, _) decoWin = focus mainw >> handleScreenCrossing mainw decoWin >> return ()
