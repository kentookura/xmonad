module Workspaces where

import qualified Data.Map                  as M

import           XMonad

import           XMonad.Actions.TopicSpace

--data WorkspaceConfig = WorkspaceConfig { dirs :: M.Map WorkspaceId Dir }

--spawnShell :: X ()
--spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn
--
--spawnShellIn :: Dir -> X ()
--spawnShellIn dir = spawn $ "alacritty --working-directory " ++ dir
--
--spawnCmdIn :: Dir -> String -> X ()
--spawnCmdIn dir cmd = spawn $ "alacritty --working-directory " ++ dir ++ " -e " ++ cmd
