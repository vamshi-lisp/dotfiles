-- | 

module Custom.MyAutoStart where

--import Custom.MyPrompt



import XMonad


import XMonad.Prompt.XMonad

import XMonad.Util.SpawnOnce

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
          --spawn ".config/brightness.sh -4"
          --xmonadPromptC mainPrompt mainPromptXPConfig
