-- | 

module Custom.Layouts where

    -- Base
import XMonad

    -- Layout
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ToggleLayouts ( toggleLayouts)
import XMonad.Layout.ResizableTile (ResizableTall(..))
    -- Hooks
import XMonad.Hooks.ManageDocks (avoidStruts) --, docksEventHook, manageDocks,ToggleStruts(..))

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
