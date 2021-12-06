module Search
  ( searchList
  ) where
import qualified Data.Map              as M
import           XMonad
import qualified XMonad.Actions.Search as S

searchList method =
  M.fromList [((0, xK_d), method S.duckduckgo), ((0, xK_h), method S.hoogle)]
