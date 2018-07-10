-- | A generalization of LIO's core components to work for any monad, instead of just IO. 

module LMonad (module LMonad) where

import LMonad.TCB as LMonad ( 
        Label (..)
      , LMonad (..)
      , LMonadT
      , runLMonad
      , runLMonadWith
      , lLift
      , getCurrentLabel
      , getClearance
      , lubCurrentLabel
      , canSetLabel
      , setLabel
      , taintLabel
      , taintLabels
      , setClearance
      , Labeled
      , label
      , unlabel
      , canUnlabel
      , labelOf
      , ToLabel(..)
      , Lattice(..)
      , swapBase
    )

-- most code should import LMonad
-- trusted code can import LMonad.TCB
--
-- You will also need to import a LMonad.Label.* module or create an instance of Label.

