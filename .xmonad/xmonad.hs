import Custom.Keys
import Custom.Layouts
import Custom.MyAutoStart
import Custom.MyProject
import Custom.Variables
import Custom.MyPrompt(mainPromptXPConfig)
import Custom.GridSelect
import Custom.Colors
import Color

    -- Base
import XMonad
import System.IO (hPutStrLn,hClose)
import qualified XMonad.StackSet as W
import System.Exit
--import PagerHints

import Data.List.Split (chunksOf)
import Test.FitSpec.PrettyPrint (columns)
import Data.Maybe (fromMaybe)

    -- Hooks
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks (docksEventHook, manageDocks)
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ServerMode
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import XMonad.Hooks.ManageDocks (avoidStruts) --, docksEventHook, manageDocks,ToggleStruts(..))
       

    -- Layout
import XMonad.Layout.ShowWName
import XMonad.Layout.ToggleLayouts (ToggleLayout(..))
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ToggleLayouts ( toggleLayouts)
import XMonad.Layout.ResizableTile (ResizableTall(..))

    -- Actions
import XMonad.Actions.Navigation2D
import XMonad.Actions.DynamicProjects
import qualified XMonad.Actions.Search as S
import XMonad.Actions.WithAll
import XMonad.Actions.SimpleDate
import XMonad.Actions.TagWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Commands (defaultCommands)

    -- Util
import XMonad.Util.EZConfig  (additionalKeysP,mkNamedKeymap)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad


    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.XMonad
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Window
import XMonad.Prompt.RunOrRaise
import PagerHints

main :: IO()
main = do

  
  xmonad $  dynamicProjects projects
         $  withNavigation2DConfig def
         $  additionalNav2DKeys (xK_Up, xK_Left, xK_Down, xK_Right)
                                    [(mod4Mask,               windowGo  ),
                                     (mod4Mask .|. shiftMask, windowSwap)]
                                    False
         $ ewmh
	$ pagerHints
         $ myConfig 
myConfig  = def
    			{ modMask     = myModMask -- Use the "Win" key for the mod key
    			, layoutHook         = showWName' myShowWNameTheme myLayouts
    			, manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
    			, handleEventHook    = serverModeEventHookCmd
                       				<+> serverModeEventHook
                       				<+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                       				<+> docksEventHook
    			, startupHook = myStartupHook
    			, terminal    = myTerminal
 			, borderWidth =  myBorderWidth         -- Sets border width for windows
    			, normalBorderColor   =  myNormalBorderColor  -- Border color of normal windows
    			, focusedBorderColor  =  myFocusedBorderColor  -- Border color of focused windows
    			, workspaces         = myWorkspaces
    			       	}  `additionalKeysP` myKeys

-- myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              ="xft:Lucida MAC:bold:size=100"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#0F0F0F"
    , swn_color             = "#d8c1e3"
    }

myWorkspaces :: [String]
myWorkspaces = ["hi!!"]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    	  where fadeAmount = 0.89
myManageHook :: ManageHook
myManageHook = composeOne
  [ className =? "Pidgin" -?> doFloat
  , className =? "XCalc"  -?> doFloat
  , className =? "mpv"    -?> doFloat
  , isDialog              -?> doCenterFloat

    -- Move transient windows to their parent:
  , transience
  ]

myKeys :: [(String,X())]
myKeys =[
       ("S-<Return>" ,  xmonadPromptC myKeys' mainPromptXPConfig)
       ,  ("S-<Tab>" ,  xmonadPromptC myKeys' mainPromptXPConfig)
       , ("M1-<Return>",  spawn myTerminal )
       , ("M-<Tab>" ,  spawnSelected' myList)
       , ("M-2",  spawn "scrot")
     ]
      
myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                --, NS "mocp" spawnMocp findMocp manageMocp
                ]
  where
    spawnTerm  = myTerminal ++ " -n scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
