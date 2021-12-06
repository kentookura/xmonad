module Utils where

import           Control.Monad
import           Data.List.Split               (splitOneOf)
import           Data.Maybe
import           System.Directory

import           XMonad                        hiding ((|||))
import           XMonad.Actions.GridSelect
import           XMonad.Actions.WorkspaceNames
import qualified XMonad.StackSet               as W
import           XMonad.Util.XUtils            (fi)

myHome :: String
myHome = "/home/kento"

curDirToWorkspacename :: X ()
curDirToWorkspacename = do
  name <- getCurrentWorkspaceName
  when (isNothing name) $ do
    dir <- io getCurrentDirectory
    when (dir /= myHome) $ do
      setCurrentWorkspaceName $ last $ splitOneOf "/" dir


colorizer :: a -> Bool -> X (String, String)
colorizer _ isFg = do
  fBC <- asks (focusedBorderColor . config)
  nBC <- asks (normalBorderColor . config)
  return $ if isFg then (fBC, nBC) else (nBC, fBC)

windowMenu :: X ()
windowMenu = withFocused $ \w -> do
  Rectangle x  y  wh  ht  <- getSize w
  Rectangle sx sy swh sht <-
    gets $ screenRect . W.screenDetail . W.current . windowset
  let originFractX = (fi x - fi sx + fi wh / 2) / fi swh
      originFractY = (fi y - fi sy + fi ht / 2) / fi sht
      gsConfig     = (buildDefaultGSConfig colorizer)
        { gs_originFractX = originFractX
        , gs_originFractY = originFractY
        }
      actions =
        [ ("Cancel menu", return ())
        , ("Close"      , kill)
        , ("alacritty"  , spawn "alacritty")
        , ("browser"    , spawn "qutebrowser")
        , ("pdfs"       , spawn "~/bin/openpdfs")
        ]
  runSelectedAction gsConfig actions

getSize :: Window -> X Rectangle
getSize w = do
  d  <- asks display
  wa <- io $ getWindowAttributes d w
  let x  = fi $ wa_x wa
      y  = fi $ wa_y wa
      wh = fi $ wa_width wa
      ht = fi $ wa_height wa
  return (Rectangle x y wh ht)
