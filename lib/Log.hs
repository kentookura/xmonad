module Log
  ( myLogHook,
    safePrintToPipe,
  )
where

import           Control.Monad                 (filterM, join, liftM)
import           Data.IORef
import qualified Data.Set                      as S
import           System.IO
import           Themes
import           XMonad                        hiding (logHook)
import           XMonad.Actions.WorkspaceNames
import           XMonad.Hooks.DynamicBars
import           XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet               as W
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run               (spawnPipe)
import           XMonad.Util.SpawnNamedPipe
import           XMonad.Util.WorkspaceCompare

xmobarFont :: Int -> String -> String
xmobarFont f = wrap (concat ["<fn-", show f, ">"]) "</fn>"

--myLogHook :: X ()
myLogHook = do
  t <- getNamedPipe "xmobar"
  --dynamicLogWithPP $ topBarPP { ppOutput  = safePrintToPipe t}
  workspaceNamesPP topBarPP {ppOutput = safePrintToPipe t} >>= dynamicLogWithPP

windowCount :: X (Maybe String)
windowCount = gets $ Just . wrap "[ " " ]" . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

topBarPP :: PP
topBarPP =
  def
    { ppTitle = xmobarColor white "" . shorten 150,
      ppCurrent = xmobarColor blue "" . wrap "[" "]",
      ppLayout = layoutMap,
      ppVisible = xmobarColor white "" . wrap "/" "/",
      ppHidden = id,
      ppSep = xmobarColor purple "" " | ",
      ppExtras = [windowCount],
      ppSort = (. namedScratchpadFilterOutWorkspace) <$> ppSort def
    }

layoutMap :: String -> String
layoutMap l = case l of
  "tiled"       -> "<icon=tile.xbm/>"
  "mirrorTiled" -> "<icon=bstack.xbm/>"
  "full"        -> "<icon=monocle.xbm/>"
  _             -> "unknown layout"

todoLogger = undefined

safePrintToPipe :: Maybe Handle -> String -> IO ()
safePrintToPipe = maybe (\_ -> return ()) hPutStrLn
