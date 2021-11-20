module Search
  (searchList)
  where
import XMonad
import qualified Data.Map as M
import qualified XMonad.Actions.Search as S

searchList method = M.fromList
       [ ((0, xK_d), method S.duckduckgo)
       , ((0, xK_h), method S.hoogle)
       ]
