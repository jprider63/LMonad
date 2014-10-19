-- Only trusted code should import this module. 

module LMonad.TCB (
        module Export
      , LMonad (..)
      , LMonadT
      , runLMonad
      , lLift
      , getCurrentLabel
      , getClearance
      , setLabel
      , taintLabel
      , setClearance
      , raiseClearanceTCB
      , lowerLabelTCB
      , Labeled
      , label
      , unlabel
      , labelOf
      , toLabeledTCB
      , ToLabel(..)
    ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Prelude

import LMonad.Label as Export

class Monad m => LMonad m where
    lFail :: m a
    lAllowLift :: m Bool
    -- lLift???

data Label l => LState l = LState {
        lStateLabel :: !l
      , lStateClearance :: !l
    }

-- | Type class to convert a given type to a given label.
class ToLabel t l where
    toConfidentialityLabel :: t -> l
    toIntegrityLabel :: t -> l

-- Transformer monad that wraps the underlying monad and keeps track of information flow. 
data (Label l, Monad m, LMonad m) => LMonadT l m a = LMonadT {
        lMonadTState :: (StateT (LState l) m a)
    }

instance (Label l, LMonad m) => Monad (LMonadT l m) where
    (LMonadT ma) >>= f = 
        LMonadT $ ma >>= (lMonadTState . f)
        -- LMonadT $ do
        -- a <- ma
        -- let (LMonadT mb) = f a
        -- mb
    return = LMonadT . return
    fail _ = LMonadT $ lift lFail

-- Runs the LMonad with bottom as the initial label and clearance. 
runLMonad :: (Label l, LMonad m) => LMonadT l m a -> m a
runLMonad (LMonadT lm) = 
    let s = LState bottom bottom in
    evalStateT lm s

-- class LMonadTrans t where 
--     lift :: (LMonad m) => m a -> t m a
-- instance (Label l) => MonadTrans (LMonadT l) where
--     -- lift :: (Monad m, LMonad m) => m a -> LMonadT l m a
--     lift m = LMonadT $ lift m

lLift :: (Label l, LMonad m) => m a -> LMonadT l m a
lLift ma = LMonadT $ do
    allow <- lift lAllowLift
    if allow then
        lift ma
    else
        lift lFail

getCurrentLabel :: (Label l, LMonad m) => LMonadT l m l
getCurrentLabel = LMonadT $ do
    (LState label _) <- get
    return label

getClearance :: (Label l, LMonad m) => LMonadT l m l
getClearance = LMonadT $ do
    (LState _ clearance) <- get
    return clearance

guardAlloc :: (Label l, LMonad m) => l -> StateT (LState l) m ()
guardAlloc l = do
    (LState label clearance) <- get
    unless (canFlowTo label l && canFlowTo l clearance) $ 
        lift lFail

setLabel :: (Label l, LMonad m) => l -> LMonadT l m ()
setLabel l = LMonadT $ do
    guardAlloc l
    (LState _ clearance) <- get
    put $ LState l clearance

taintLabel :: (Label l, LMonad m) => l -> LMonadT l m ()
taintLabel l1 = do
    l2 <- getCurrentLabel
    let l = lub l1 l2
    setLabel l

setClearance :: (Label l, LMonad m) => l -> LMonadT l m ()
setClearance c = LMonadT $ do
    guardAlloc c
    (LState label _) <- get
    put $ LState label c

-- TODO: Does this diverge from LIO's formalism?
-- Sets the current clearance to the join of the old clearance and the given clearance.
raiseClearanceTCB :: (Label l, LMonad m) => l -> LMonadT l m ()
raiseClearanceTCB c = LMonadT $ do
    (LState label clearance) <- get
    put $ LState label $ lub clearance c

-- TODO: I think this does diverge from LIO
-- Sets the current label to the meet of the old label and the given label.
lowerLabelTCB :: (Label l, LMonad m) => l -> LMonadT l m ()
lowerLabelTCB l = LMonadT $ do
    (LState label clearance) <- get
    put $ LState (glb label l) clearance

-- Labeled values.
data Label l => Labeled l a = Labeled {
        labeledLabel :: l
      , labeledValue :: a
    }

label :: (Label l, LMonad m) => l -> a -> LMonadT l m (Labeled l a)
label l a = LMonadT $ do
    guardAlloc l
    return $ Labeled l a

unlabel :: (Label l, LMonad m) => Labeled l a -> LMonadT l m a
unlabel l = do
    setLabel $ labelOf l
    return $ labeledValue l

labelOf :: Label l => Labeled l a -> l
labelOf = labeledLabel

-- TODO: I'm pretty sure this also differs from their semantics
-- Should this also be TCB? Could potentially leak via side channel since lifting might be allowed. 
toLabeledTCB :: (Label l, LMonad m) => l -> LMonadT l m a -> LMonadT l m (Labeled l a)
toLabeledTCB l ma = do
    oldLabel <- getCurrentLabel
    oldClearance <- getClearance
    raiseClearanceTCB l
    a <- ma
    la <- label l a
    lowerLabelTCB oldLabel
    setClearance oldClearance
    return la

