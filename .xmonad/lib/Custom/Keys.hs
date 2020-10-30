

module Custom.Keys where

    -- Custom
import Custom.MyPrompt
import Custom.Variables

    -- Base
import XMonad
import System.Exit
--import qualified XMonad.StackSet as W

    -- Hooks
import XMonad.Hooks.ManageDocks (ToggleStruts(..))

    -- Layout
import XMonad.Layout.ToggleLayouts (ToggleLayout(..))

    -- Prompt
--import XMonad.Prompt
--import XMonad.Prompt.Shell
--import XMonad.Prompt.Window
--import XMonad.Prompt.XMonad
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Window
--import XMonad.Prompt.Workspace
--import XMonad.Prompt.ConfirmPrompt
--import XMonad.Prompt.AppLauncher as AL

    -- Actions
import XMonad.Actions.WithAll
import XMonad.Actions.SimpleDate
import XMonad.Actions.TagWindows
import qualified XMonad.Actions.Search as S
--import qualified XMonad.Actions.TreeSelect as TS ( treeselectWorkspace )
import XMonad.Actions.CycleWS
--import XMonad.Actions.GridSelect
--import XMonad.Actions.CopyWindow(copy)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.DynamicProjects( switchProjectPrompt,  shiftToProjectPrompt )

myKeys' :: [(String,X())]
myKeys' = [  
  ("restart xmonad" ,spawn "xmonad --recompile ; xmonad --restart")
      , ("toggle full screen (not taffybar)", sendMessage (Toggle "Full"))   
      , ("delete one window" , {-addName "Kill One Window" $ -} kill)
      , ("kill a workspace" , {- addName "Kill WorkSpace" $ -} removeWorkspace )
      , ("kill all windows",  killAll)                          
      --, ("M-w",{- addName "Main prompt" $ -} xmonadPromptC mainPrompt mainPromptXPConfig )
      , ("myshell",  spawn "emacsclient -c -a '' --eval '(eshell)'")             
      , ("terminal", {-addName "Konsole" $ -} spawn myTerminal )      
      , ("next workspace",{- addName "next ws" $ -} nextWS)
      , ("prev workspace", {- addName "prev ws" $ -} prevWS)
 --     , ("M-2", addName "capture screen" $ spawn "scrot")
      ,  ("exit shutdown system", spawn "poweroff")
      ,  ("exit reboot system", spawn "reboot")
      ,  ("exit logout xmonad wm", io $ exitWith ExitSuccess)
      ,  ("switchPrompt" , switchProjectPrompt  switchProjectXPConfig)
      ,  ("runOrRaisePrompt" , runOrRaisePrompt runOrRaiseXPConfig)
      ,  ("windowPrompt", windowPrompt myXPConfig'  Goto allWindows)
      ,  ("throwWindowPrompt" , shiftToProjectPrompt myXPConfig)     
      ]
