module Projects
  ( projects
  )
where

import           XMonad.Actions.DynamicProjects

projects =
  [ Project { projectName = "hakyll"
            , projectDirectory = "~/hakyll-nix-template"
            , projectStartHook = Nothing
            }
  ]

