-- | 

module Custom.MyProject where


    -- Base
import XMonad

    -- Action
import XMonad.Actions.DynamicProjects


projects :: [Project]
projects =[ Project { projectName      = "browser"
            , projectDirectory = "~/Download"
            , projectStartHook = Just $ do  spawn "brave"
            }

  , Project { projectName      = "editor"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do  spawn "emacs"
            }
  , Project { projectName      = "terminal"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do  spawn "konsole"
            }
  , Project { projectName      = "library"
            , projectDirectory = "~/books"
            , projectStartHook =  Nothing --  spawn "konsole"
            }
 {-, Project { projectName      = "spotify"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do  spawn "flatpak run com.spotify.Client"
           -}
    
  ]
