-- | 

module Custom.MyPrompt where

    -- Custom
import Custom.Variables
import Custom.Colors

import Color

    -- Base
import XMonad
import System.Exit
     
    -- Action
import qualified XMonad.Actions.Search as S
import XMonad.Actions.DynamicProjects

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.XMonad
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Window
import XMonad.Prompt.RunOrRaise

------------------------------------------------------------------------
-- SEARCH ENGINES
------------------------------------------------------------------------
-- Xmonad has several search engines available to use located in
-- XMonad.Actions.Search. Additionally, you can add other search engines
-- such as those listed below.
archwiki, ebay, news, reddit, urban :: S.SearchEngine

archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
ebay     = S.searchEngine "ebay" "https://www.ebay.com/sch/i.html?_nkw="
news     = S.searchEngine "news" "https://news.google.com/search?q="
reddit   = S.searchEngine "reddit" "https://www.reddit.com/search/?q="
urban    = S.searchEngine "urban" "https://www.urbandictionary.com/define.php?term="
nixos    = S.searchEngine "nixos" "https://search.nixos.org/packages"
-- This is the list of search engines that I want to use. Some are from
-- XMonad.Actions.Search, and some are the ones that I added above.
searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", archwiki)
             , ("d", S.duckduckgo)
             , ("e", ebay)
             , ("g", S.google)
             , ("h", S.hoogle)
             , ("i", S.images)
             , ("n", news)
             , ("r", reddit)
             , ("s", S.stackage)
             , ("t", S.thesaurus)
             , ("v", S.vocabulary)
             , ("b", S.wayback)
             , ("u", urban)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             , ("z", S.amazon)
             ]


--------------------------------------------------------------------------------
-- | Customize the way 'XMonad.Prompt' looks and behaves.  It's a
-- great replacement for dzen.
myXPConfig :: XPConfig
myXPConfig = def
  {position            = CenteredAt { xpCenterY = 0.5, xpWidth = 1 }
  , bgColor           = "#000000"
  , fgColor           = "#DDDDDD"
  , fgHLight          = "#FFFFFF"
  , bgHLight          = "#333333"
  , borderColor       = "#FFFFFF"
  , alwaysHighlight   = True
  , promptBorderWidth = 1
  , font              = "xft:LucidaGrande:size=34"
  , height            = 130
  , searchPredicate   = fuzzyMatch
  }

appXPConfig :: XPConfig
appXPConfig = def
                {
                                     position            = CenteredAt { xpCenterY = 0.2, xpWidth = 0.5 }
                                     , bgColor           = "grey7"
                                     , fgColor           = "grey80"
                                     , bgHLight          = "#02bfa0"
                                     , fgHLight          = "White"
                                     , borderColor       = "white"
                                     , alwaysHighlight   = True
                                     , promptBorderWidth = 4
                                     , defaultText       = []
                                     , font              = "xft:LucidaGrande:size=34"
                                     , height            = 96
                                     , searchPredicate   = fuzzyMatch
                                     }

myXPConfig' :: XPConfig
myXPConfig' = appXPConfig
             {
               autoComplete      = Just 100000    -- set Just 100000 for .1 sec
               --, position          = CenteredAt { xpCenterY = 2.85, xpWidth = 0.7 }
             }

searchPrompt :: X()
searchPrompt = inputPrompt myXPConfig "yay " ?+ appStore

appStore :: String -> X()
appStore query =  spawn (((myTerminal ++) " -e yay " ++ ) query)



mainPromptXPConfig :: XPConfig
mainPromptXPConfig = myXPConfig' {
                       font              = "xft:LucidaMAC:size=26"
                       , bgHLight        =   showWebColor blue --"#a4c20e"
                       , fgHLight           = "#ffffff"
                       , fgColor           = "#E0D8D8" --"#ffffff"--showWebColor cyan--"#ffffff"  
                       , promptBorderWidth = 6
                      , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.34}
                     --  , position           =  Bottom
                       , height            = 100
                       , alwaysHighlight     = True
                       , borderColor         =  showWebColor cyan --"#e0dfde"
                       , bgColor             =   showWebColor base03 --"#0ba3a3"
                         }
switchProjectXPConfig :: XPConfig
switchProjectXPConfig = mainPromptXPConfig {
                                  bgColor           = "#13850f"
                                , fgColor          = "#e0dfde"
                                , bgHLight          = "#2a7d30"
                                , position          = Top
                                , height            = 120
                                , promptBorderWidth = 4
                                , font              = "xft:LucidaGrande:size=27"
                                , borderColor       = "#000000"
                                  }
iconicXPConfig :: XPConfig
iconicXPConfig = mainPromptXPConfig {
                   font              = "xft:FontAwesome:size=31"
                  }
runOrRaiseXPConfig :: XPConfig
runOrRaiseXPConfig = appXPConfig {
                                  bgColor = "#e0dfde"
                                , borderColor = "#000000"
                                , fgColor = "#0f0f0f"
                                 }
-- A list of all of the standard Xmonad prompts and a key press assigned to them.
-- These are used in conjunction with keybinding I set later in the config.
mainPrompt :: [(String , X ())]
mainPrompt = [("switchPrompt" , switchProjectPrompt  switchProjectXPConfig)
             , ("runOrRaisePrompt" , runOrRaisePrompt runOrRaiseXPConfig)
             , ("windowPrompt", windowPrompt myXPConfig'  Goto allWindows)
             , ("throwWindowPrompt" , shiftToProjectPrompt myXPConfig)
             --, ("playStore", searchPrompt)
             , ("exitPrompt" ,xmonadPromptC exitPrompt exitPromptXPConfig)
             ]
exitPromptXPConfig :: XPConfig
exitPromptXPConfig = myXPConfig' {
                       position = CenteredAt {xpCenterY = 0.3 , xpWidth = 0.84}
                       , height            = 200
                       , bgHLight          = "#0f0f0f"
                       , bgColor           = "#000000"
                       , promptBorderWidth = 3
                       , font              = "xft:LucidaGrande:size=34"
                     }
exitPrompt :: [(String, X ())]
exitPrompt = [
        ("shutdown", spawn "poweroff"),
        ("reboot", spawn "reboot"),
        ("exit", io $ exitWith ExitSuccess),
        ("xRestart", restart "xmonad" True)
    ]
