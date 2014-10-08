-- | A generalization of LIO's core components to work for any monad, instead of just IO. 

module LMonad (module LMonad) where

import LMonad.TCB as LMonad ( 
        Label (..)
      , LMonad (..)
      , LMonadT
      , runLMonad
      , lLift
      , getCurrentLabel
      , getClearance
      , setLabel
      , taintLabel
      , setClearance
      , Labeled
      , label
      , unlabel
      , labelOf
    )

-- most code should import LMonad
-- trusted code can import LMonad.TCB
--
-- You will also need to import a LMonad.Label.* module or create an instance of Label.
