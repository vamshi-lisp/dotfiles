module Custom.Variables where

    -- Base
import XMonad
import qualified XMonad.StackSet as W


myModMask :: KeyMask
myModMask = mod4Mask

--myFont2 :: String
--myFont2 = "xft:Lucida Grande:size=13:antialias=true:hinting=true"

myFont1 :: String
myFont1 = "xft:Lucida MAC:bold:size=14:antialias=true:hinting=true"

myFont2 :: String
myFont2 = "xft:Lucida Grande:bold:size=65:antialias=true:hinting=true"

--myAltMask :: KeyMask
--myAltMask = mod1Mask

myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:size=20"

myTerminal :: String
myTerminal = "konsole"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

--myBrowser :: String
--myBrowser = "brave"
myBorderWidth :: Dimension  
myBorderWidth = 2          -- Sets border width for windows

myNormalBorderColor ::  String
myNormalBorderColor   = "#292d3e"  -- Border color of normal windows



myFocusedBorderColor  :: String   -- Border color of focused windo
myFocusedBorderColor  = "#bbc5ff"  -- Border color of focused windows
