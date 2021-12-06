--{-# LANGUAGE FlexibleInstances     #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE NamedFieldPuns        #-}
--
module ConfigurableBar
--  ( BarConfiguration
--  , BarConfiguration'
--  , myBC
--  , processBarConfig
--  ) where
                        where
--
--import           Data.List
--
--import           XMonad
--import           XMonad.Layout
--import           XMonad.Layout.Decoration       (Decoration, DecorationStyle,
--                                                 Shrinker, decorate)
--import           XMonad.Layout.DecorationAddons
--import           XMonad.Layout.LayoutModifier
--
--
--clickHandler
--  :: BarConfiguration' -> Window -> ds a Window -> Int -> Int -> X Bool
--clickHandler bc mainw w i _ = do
--  let action = find (\t -> fst' t > i) (leftButtons bc)
--  case action of
--    Nothing  -> return True
--    Just val -> focus mainw >> thd val >> return True
--
--configurableBar :: BarConfiguration -> ModifiedLayout (Decoration ds s) l a
--configurableBar bc = decoration (Shrinker DefaultShrinker) DefaultTheme
--
--newtype WithConfigurableBar a = ConfigurableBar BarConfiguration'
--
--instance Eq a => DecorationStyle WithConfigurableBar a where
--  decorationCatchClicksHook _ mainw dFL dFR = clickHandler
--  decorationAfterDraggingHook _ (mainw, _) decoWin =
--    focus mainw >> handleScreenCrossing mainw decoWin >> return ()
--
--data BarConfiguration = BarConfiguration
--  { leftIcons    :: [(Icon, X ())]
--  , rightIcons   :: [(Icon, X ())]
--  , barHeight    :: Int
--  , iconSize     :: Int
--  , leftPadding  :: Int
--  , rightPadding :: Int
--  }
--
--data BarConfiguration' = BarConfiguration'
--  { leftButtons  :: [(Int, Icon, X ())]
--  , rightButtons :: [(Int, Icon, X ())]
--  }
--
--myBC = BarConfiguration { leftIcons    = [([[1]], return ())]
--                        , rightIcons   = [([[1]], return ())]
--                        , barHeight    = 10
--                        , iconSize     = 20
--                        , leftPadding  = 5
--                        , rightPadding = 5
--                        }
--
--processBarConfig :: BarConfiguration -> BarConfiguration'
--processBarConfig bc = BarConfiguration'
--  { leftButtons  = [ (i, li, x)
--                   | i       <-
--                     [ leftPadding bc + is | is <- map (* iconSize bc) [0 ..] ]
--                   , (li, x) <- leftIcons bc
--                   ]
--  , rightButtons =
--    [ (i, li, x)
--    | i <- reverse [ rightPadding bc + is | is <- map (* iconSize bc) [0 ..] ]
--    , (li, x) <- rightIcons bc
--    ]
--  }
--
--fst' :: (a, b, c) -> a
--fst' (a, _, _) = a
--
--thd :: (a, b, c) -> c
--thd (_, _, c) = c
--
--
--type Icon = [[Int]]
--
--convertToBool :: Icon -> [[Bool]]
--convertToBool = map (map (== 1))
--
--toggleKbd :: String
--toggleKbd =
--  "dbus-send --type=method_call --dest=org.onboard.Onboard /org/onboard/Onboard/Keyboard org.onboard.Onboard.Keyboard.ToggleVisible"
--
----myDeco
----  :: (Eq a, Shrinker s)
----  => s
----  -> Theme
----  -> l a
----  -> ModifiedLayout (Decoration ImageButtonDecoration s) l a
----myDeco s c = decoration s c $ NFD True
--
---- | A function intended to be plugged into the 'decorationCatchClicksHook' of a decoration.
---- It will intercept clicks on the buttons of the decoration and invoke the associated action.
---- To actually see the buttons, you will need to use a theme that includes them.
---- See 'defaultThemeWithImageButtons' below.
----imageTitleBarButtonHandler :: Window -> Int -> Int -> X Bool
----imageTitleBarButtonHandler mainw distFromLeft distFromRight = do
----  let action
----        | fi distFromLeft <= menuButtonOffset + buttonSize
----        = spawn toggleKbd >> return True
----        | fi distFromRight
----          >= closeButtonOffset
----          && fi distFromRight
----          <= closeButtonOffset
----          +  buttonSize
----        = focus mainw >> kill >> return True
----        | fi distFromRight
----          >= maximizeButtonOffset
----          && fi distFromRight
----          <= maximizeButtonOffset
----          +  buttonSize
----        = focus mainw >> sendMessage (maximizeRestore mainw) >> return True
----        | fi distFromRight
----          >= minimizeButtonOffset
----          && fi distFromRight
----          <= minimizeButtonOffset
----          +  buttonSize
----        = focus mainw >> minimizeWindow mainw >> return True
----        | otherwise
----        = focus mainw >> windowMenu >> return True >> return True
----  action