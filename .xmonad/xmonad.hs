--------------------------------------------------------------------------------
-- | Example.hs
--
-- Example configuration file for xmonad using the latest recommended
-- features (e.g., 'desktopConfig').
module Main (main) where

--------------------------------------------------------------------------------
import Custom.Keys
{-import Custom.Layouts
-}
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
--import XMonad.Util.NamedActions


    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.XMonad
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Window
import XMonad.Prompt.RunOrRaise
import PagerHints
--import XMonad.Config.DescriptiveKeys
--import Zenity
--------------------------------------------------------------------------------
main :: IO()
main = do

 -- xmproc <- (spawnPipe "xmobar  /home/vamshi/.xmobarrc" )
  
  xmonad $  dynamicProjects projects
         $  withNavigation2DConfig def
         $  additionalNav2DKeys (xK_Up, xK_Left, xK_Down, xK_Right)
                                    [(mod4Mask,               windowGo  ),
                                     (mod4Mask .|. shiftMask, windowSwap)]
                                    False
         $ ewmh
         -- $ addDescrKeys' ((mod4Mask, xK_Return ) , showKeybindings) myKeys
	-- $  addDescrKeys' ((mod4Mask, xK_Return), showKeybindings) (myKeys $ myXPConfig darkmode)
	-- $ addDescrKeys' (myCheatsheetKey, showKeybindings) myKeys
	$ pagerHints
         $ myConfig 


myConfig  = def
    			{ modMask     = myModMask -- Use the "Win" key for the mod key
    			, layoutHook         = showWName' myShowWNameTheme myLayouts
    			, manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
    -- Run xmonad commands from command line with "xmonadctl command". Commands include:
    -- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
    -- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
    -- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
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
    			{-, logHook = workspaceHistoryHook <+> myLogHook <+> dynamicLogWithPP xmobarPP
                        { ppOutput = \x -> hPutStrLn xmproc0 x -- >> hPutStrLn xmproc1 x  >> hPutStrLn xmproc2 x
                        	, ppCurrent = xmobarColor "#c3e88d" "" . wrap "[" "]" -- Current workspace in xmobar
                        	, ppVisible = xmobarColor "#c3e88d" ""                -- Visible but not current workspace
                        --, ppHidden = xmobarColor "#82AAFF" ""
                        	, ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                        -- , ppHiddenNoWindows = xmobarColor "#F07178" ""        -- Hidden workspaces (no windows)
                        	, ppHiddenNoWindows= \( _ ) -> ""       -- Only shows visible workspaces. Useful for TreeSelect.
                        	, ppTitle = xmobarColor "#d0d0d0" "" . shorten 60     -- Title of active window in xmobar
                        	, ppSep =  "<fc=#666666> | </fc>"                     -- Separators in xmobar
                        	, ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
                        	, ppExtras  = [windowCount]                           -- # of windows current workspace
                        	, ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
                        }  -}
       	}  `additionalKeysP` myKeys --[ ("M-<SPC>" , xmonadPromptC myKeys' mainPromptXPConfig )]

{-
myStartupHook :: X ()
myStartupHook = do
          spawnOnce "~/.fehbg"
          spawnOnce "picom &"
	  spawnOnce "status-notifier-watcher &"
	  spawnOnce "gtk-sni-tray-standalone &"
	  spawnOnce "imalison-taffybar &"
          --spawnOnce "nm-applet &"
          --spawnOnce "volumeicon &"
          --spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x292d3e --height 18 &"
          spawnOnce "/usr/bin/emacs --daemon &"
          -- spawnOnce "kak -d -s mysession &"
          spawnOnce "redshift-gtk"
	  spawnOnce "/home/vamshi/repo/legend-dotfiles/bin/wm/lowbatt"
          --spawn ".config/brightness.sh -4"
       --   xmonadPromptC mainPrompt mainPromptXPConfig

myModMask :: KeyMask
myModMask = mod4Mask

--myFont2 :: String
--myFont2 = "xft:Lucida Grande:size=13:antialias=true:hinting=true"

myFont1 :: String
myFont1 = "xft:Lucida MAC:bold:size=14:antialias=true:hinting=true"

myFont2 :: String
myFont2 = "xft:Lucida Grande:bold:size=65:antialias=true:hinting=true"

myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:size=20"

myTerminal :: String
myTerminal = "konsole"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

--myBrowser :: String
--myBrowser = "brave"
myBorderWidth :: Dimension  
myBorderWidth = 2        -- Sets border width for windows

myNormalBorderColor ::  String
myNormalBorderColor   = "#292d3e"  -- Border color of normal windows



myFocusedBorderColor  :: String   -- Border color of focused windo
myFocusedBorderColor  = "#bbc5ff"  -- Border color of focused windows

-}

-- Theme for showWName which prints current workspace when you change workspaces.
myShowWNameTheme :: SWNConfig
myShowWNameTheme = def
    { swn_font              ="xft:Lucida MAC:bold:size=100"
    , swn_fade              = 1.0
    , swn_bgcolor           = "#0F0F0F"
    , swn_color             = "#d8c1e3"
    }

myWorkspaces :: [String]
myWorkspaces = ["srort"]

{-projects :: [Project]
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
 , Project { projectName      = "spotify"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do  spawn "spotify"
           }
  ]
  -}

-- Sets opacity for inactive (unfocused) windows. I prefer to not use
-- this feature so I've set opacity to 1.0. If you want opacity, set
-- this to a value of less than 1 (such as 0.9 for 90% opacity).
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    	  where fadeAmount = 0.89

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
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
     ]
      

{-
myKeys :: XConfig l ->  [((KeyMask, KeySym), NamedAction)]
myKeys conf =  let
           subKeys name list = subtitle name : mkNamedKeymap conf list

	   in
        subKeys "IO Actions"
        [("M-q" , addName "                  restart XMonad" $ spawn "xmonad --recompile ; xmonad --restart")
        -- Exit XMonad
       -- ("M-<Esc>",   confirmPrompt myXPConfig "exit" (io exitSuccess))
      --, ("M1-q", spawn "./repos/xmenu/xmenu.sh")          -- XMenu (Custom)
      , ("M-f", addName "                   full Screen" $  sendMessage (Toggle "Full"))          -- Full screen (Not removes xmobar)
      , ("M1-<Backspace>" , addName "            kill One Window" $  kill)
      , ("M-<Backspace>" ,  addName "          kill WorkSpace" $  removeWorkspace )
      , ("M1-S-<Backspace>", addName "         kill all Windows" $  killAll)                                  -- Kill Current Window
   --   , ("M1-d" , date) 
     --, ("M-u" , withFocused (addTag "abc"))
     -- , ("M-w", addName "                  main prompt" $  xmonadPromptC mainPrompt mainPromptXPConfig )
      --, ("M-<Esc>", spawn "clearine" )
      --, ("M1-y", searchPrompt)
      --, ("M-<Tab>" ,   namedScratchpadAction myScratchPads "terminal")
      ,  ("S-<Return>" , addName "mass " $ xmonadPromptC myKeys' mainPromptXPConfig)
      ,  ("S-<Tab>" , addName "marana mass " $ xmonadPromptC myKeys' mainPromptXPConfig)
      	 , ("M-e",  addName "                  emacs shell" $  spawn "emacsclient -c -a '' --eval '(eshell)'")                            -- Run Emacs
    	 --  , ("M-S-r", spawn "xmonad --recompile ; xmonad --restart")        -- Restarts xmonad
      	     , ("M1-<Return>", addName "               launch terminal" $  spawn myTerminal )                 -- Terminal
      , ("M-S-<Space>", addName "remove status bar" $ sendMessage ToggleStruts)         -- Toggles struts
      , ("M1-]", addName "            next ws" $  nextWS)
      , ("M1-[", addName "             prev ws" $  prevWS)
      , ("M-<Tab>" , addName "   help"  $ spawnSelected' myList)
 --     , ("M-2", addName "capture screen" $ spawn "scrot")
      --, ("M1-<Tab>" ,  goToSelected $ mygridConfig myColorizer)  -- goto selected window
      --, ("C-g b" , bringSelected $ mygridConfig myColorizer) -- bring selected window
      --,( "M1-a" , AL.launchApp appXPConfig "xdg-open" )
--      , ("C-t t", TS.treeselectWorkspace tsDefaultConfig myWorkspaces W.greedyView)
      -- tree select choose workspace to send window
--      , ("C-t g", TS.treeselectWorkspace tsDefaultConfig myWorkspaces W.shift)
      ]
      -- Appending search engine prompts to keybindings list.
      -- Look at "search engines" section of this config for values for "k".
      --  ++ [("M-s " ++ k, S.promptSearch myXPConfig f) | (k,f) <- searchList ]
      -- ++ [("M-S-s " ++ k, S.selectSearch f) | (k,f) <- searchList ]
  

-- Keybinding to display the keybinding cheatsheet
myCheatsheetKey :: (KeyMask, KeySym)
myCheatsheetKey = (myModMask .|. shiftMask, xK_slash)

-- Number of colomns with with which to display the cheatsheet
myCheatsheetCols :: Int
myCheatsheetCols = 3

formatList :: [String] -> String
formatList list = columns "SeparatorPlaceholder" -- Normalise column widths -> Table
                $ map unlines -- Connect the sublists with line breals -> [column1,column2,...]
                $ chunksOf (myCheatsheetRows (list))
                $ list -- The list to be formatted

        where rowsFromColumns list nCol = 1 + length list `div` nCol
              myCheatsheetRows list = rowsFromColumns list myCheatsheetCols


rofiKeys :: String
rofiKeys = "rofi -dmenu -p 'Keys'  -bw 0 -padding 39  -lines 12 -line-margin 3 -width 60 -xoffset 43 -yoffset 0 -location 0  -font 'Lucida MAC 17' -hide-scrollbar true -color-enabled true -color-window \" \"#99200000\", \"#59000000\", \"#00000000\" \" -color-normal \" \"#00000000\" ,\"#FFFFFF\" , \"#00000000\" , \"#59000000\", \"#1e88e5\" \" -color-active \" \"59000000\", \"#00897b\" , \"#00000000\" , \"#59000000\" ,\"#1e88e5\" \" -color-urgent \" \"#59000000\" ,\"#fdd835\" , \"#00000000\" , \"#59000000\", \"#1e88e5\"\" "

-- How to display the cheatsheet (adapted from Ethan Schoonover's config)
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings myKeyList = addName "Show Keybindings" $ io $ do
    handle <-   spawnPipe  rofiKeys
    hPutStrLn handle "TitlePlaceholder\n" -- Replaced in the script
    hPutStrLn handle $ unlines  (showKm myKeyList)
    hClose handle
    return ()
-}

--------------------------------------------------------------------------------
-- | Customize layouts.
--
-- This layout configuration uses two primary layouts, 'ResizableTall'
-- and 'BinarySpacePartition'.  You can also use the 'M-<Esc>' key
-- binding defined above to toggle between the current layout and a
-- full screen layout.
myLayouts = avoidStruts $ toggleLayouts (noBorders Full) others
  where
    others = ResizableTall 1 (1.5/100) (3/5) [] ||| emptyBSP




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
