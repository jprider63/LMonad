module LMonad.Label where

import Prelude

class Label l where
    -- Join
    lub :: l -> l -> l
    -- Meet
    glb :: l -> l -> l
    canFlowTo :: l -> l -> Bool
    bottom :: l
