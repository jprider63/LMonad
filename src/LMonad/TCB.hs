{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeFamilies #-}

-- Only trusted code should import this module. 

module LMonad.TCB (
        module Export
      , LMonad (..)
      , LMonadT
      , runLMonad
      , lLift
      , getCurrentLabel
      , getClearance
      , lubCurrentLabel
      , canSetLabel
      , setLabel
      , taintLabel
      , setClearance
      , raiseClearanceTCB
      , lowerLabelTCB
      , Labeled
      , label
      , unlabel
      , canUnlabel
      , labelOf
      , toLabeledTCB
      , ToLabel(..)
      , swapBase
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.State
import Data.Monoid
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

instance (Label l, LMonad m, Functor m) => Functor (LMonadT l m) where
    fmap f (LMonadT ma) = LMonadT $ fmap f ma

instance (Label l, LMonad m, Functor m) => Applicative (LMonadT l m) where
    pure = return
    (<*>) = ap
    
instance (Label l, LMonad m, MonadIO m) => MonadIO (LMonadT l m) where
    liftIO ma = lLift $ liftIO ma

instance (LMonad m, Label l, Functor m, MonadBase IO m) => MonadBase IO (LMonadT l m) where
    liftBase = lLift . liftBase

instance (Label l, LMonad m, MonadThrow m) => MonadThrow (LMonadT l m) where
    throwM = lLift . throwM

-- TODO: This allows replay attacks. Ie, malicious code could reset the current label or clearance to a previous value. 
--     This is needed for Database.Persist.runPool
instance (LMonad m, Label l, MonadBaseControl IO m) => MonadBaseControl IO (LMonadT l m) where
    newtype StM (LMonadT l m) a = StMT {unStMT :: StM (StateT (LState l) m) a}
    liftBaseWith f = LMonadT $ liftBaseWith $ \run -> f $ liftM StMT . run . lMonadTState
    restoreM = LMonadT . restoreM . unStMT

instance (LMonad m, Label l, Monoid (m a)) => Monoid (LMonadT l m a) where
    mempty = lLift mempty
    mappend a b = a >> b 
--    do
--        a' <- a 
--        b' <- b
--        return $ mappend a b
    
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

lubCurrentLabel :: (Label l, LMonad m) => l -> LMonadT l m l
lubCurrentLabel l = do
    getCurrentLabel >>= (return . (lub l))

getClearance :: (Label l, LMonad m) => LMonadT l m l
getClearance = LMonadT $ do
    (LState _ clearance) <- get
    return clearance

canAlloc :: (Label l, LMonad m) => l -> StateT (LState l) m Bool
canAlloc l = do
    (LState label clearance) <- get
    return $ canFlowTo label l && canFlowTo l clearance

guardAlloc :: (Label l, LMonad m) => l -> StateT (LState l) m ()
guardAlloc l = do
    guard <- canAlloc l
    unless guard $ 
        lift lFail

canSetLabel :: (Label l, LMonad m) => l -> LMonadT l m Bool
canSetLabel = LMonadT . canAlloc

setLabel :: (Label l, LMonad m) => l -> LMonadT l m ()
setLabel l = LMonadT $ do
    guardAlloc l
    (LState _ clearance) <- get
    put $ LState l clearance

taintLabel :: (Label l, LMonad m) => l -> LMonadT l m ()
taintLabel l = do
    lubCurrentLabel l >>= setLabel

-- canTaintLabel :: (Label l, LMonad m) => l -> LMonadT l m Bool
-- canTaintLabel l = do
--     lubCurrentLabel l >>= (LMonadT . canAlloc)

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

canUnlabel :: (Label l, LMonad m) => Labeled l a -> LMonadT l m Bool
canUnlabel l = do
    clearance <- getClearance
    return $ canFlowTo (labelOf l) clearance

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

swapBase :: (Label l, LMonad m, LMonad n) => (m (a,LState l) -> n (b,LState l)) -> LMonadT l m a -> LMonadT l n b
swapBase f (LMonadT m) = LMonadT $ do
    prev <- get
    ( res, new) <- lift $ f $ (runStateT m) prev
    put new
    return res
