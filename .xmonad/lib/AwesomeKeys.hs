module AwesomeKeys where

import XMonad
import XMonad.Prompt
import XMonad.Actions.Commands (defaultCommands)
import Data.Maybe (fromMaybe)


data Display = Display

instance XPrompt Display where
      showXPrompt Display = "My Keys:  " 

keyPrompt :: String -> XPConfig -> X()
keyPrompt commands  c = mkXPrompt



