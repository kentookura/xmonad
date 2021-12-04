module Topics
  ( myTopics,
    myTopicConfig,
    spawnShell,
    spawnShellIn,
    spawnCmdIn,
    goto,
    promptedGoto,
    promptedShift,
  )
where

import qualified Data.Map as M
import Themes
import XMonad
import XMonad.Actions.TopicSpace
import XMonad.Prompt.Workspace
import XMonad.StackSet as W

myTopics :: [Topic]
myTopics =
  [ "~",
    "xm",
    "dev",
    "lambda",
    "config",
    "cv",
    "docs",
    "hsk",
    "site",
    "wiki",
    "web"
  ]

myTopicConfig :: TopicConfig
myTopicConfig =
  def
    { -- associate directory with topic
      topicDirs =
        M.fromList
          [ ("~", "/home/kento"),
            ("xm", ".config/nixpkgs/xmonad"),
            ("dev", "traivelling-frontend"),
            ("lambda", "lambda"),
            ("config", ".config"),
            ("cv", "doc/cv"),
            ("docs", "doc"),
            ("hsk", "hsk"),
            ("site", "site"),
            ("web", "dl"),
            ("wiki", "wiki")
          ],
      topicActions =
        M.fromList
          [ ( "config",
              spawn "alacritty"
                >> spawn "alacritty"
            ),
            ("xm", spawn "alacritty -e vim .xmonad/xmonad.hs"),
            ( "cv",
              spawn "zathura doc/cv/output/resume.pdf"
                >> spawn "alacritty --working-directory doc/cv/markdown/"
                >> spawn "alacritty -e vim doc/cv/markdown/resume.md"
            ),
            ("docs", spawn "zathura ~/doc/haskell.pdf"),
            ( "site",
              spawn "alacritty --working-directory site/src"
                >> spawn "alacritty --working-directory site/src/templates"
                >> spawn "qutebrowser http://localhost:8000"
            ),
            ( "lambda",
              spawn "alacritty --working-directory uni -e abduco -A uni-session dvtm"
                >> spawn "zathura ~/uni/log/script.pdf"
                >> spawn "zathura ~/uni/alg/Algebra_2_13.pdf"
            ),
            ("wiki", spawn "alacritty -e vim wiki/index.md")
          ]
    }

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "alacritty --working-directory " ++ dir

spawnCmdIn :: Dir -> String -> X ()
spawnCmdIn dir cmd = spawn $ "alacritty --working-directory " ++ dir ++ " -e " ++ cmd

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt promptTheme goto

promptedShift :: X ()
promptedShift = workspacePrompt promptTheme $ windows . W.shift
