-- | 

module Custom.GridSelect where

import Custom.Variables(myFont)

import XMonad

import XMonad.Actions.GridSelect

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 150
                   , gs_cellwidth    = 340
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

myList :: [(String,String)]
myList = [("Shift+Return  Main menu","")
         ,("Shift+Tab     Main menu","")
         ,("Alt+Return    Exec terminal","")
	 ,("Mod4+2        Screenshot",""   )
         ]
