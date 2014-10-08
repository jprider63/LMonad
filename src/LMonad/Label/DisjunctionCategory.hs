-- | Reference: 
--  http://www.scs.stanford.edu/~deian/pubs/stefan:2011:dclabels.pdf
--  http://hackage.haskell.org/package/lio-0.11.5.0/docs/src/LIO-DCLabel.html
--
-- TODO: Some licensing stuff? XXX

module LMonad.Label.DisjunctionCategory where

import Data.Set (Set)
import qualified Data.Set as Set
import Prelude

import LMonad

-- TODO: Add bloom filter?? It's probably not worth it with principals usually on the order of 10.
-- data Ord p => Disjunction p = Disjunction (Set p)
type Disjunction p = Set p
type Conjunction p = Set (Disjunction p)

-- | Disjunction category label of principals in conjunction normal form. 
data Ord p => DCLabel p = DCLabel {
        dcConfidentiality :: Conjunction p
      , dcIntegrity :: Conjunction p
    }

-- TODO: normalize, implies XXX

-- TODO: Can we short circuit these folds?
forall :: (a -> Bool) -> Set a -> Bool
forall pred = Set.foldr' (\el -> ( && ) (pred el)) True

exists :: (a -> Bool) -> Set a -> Bool
exists pred = Set.foldr' (\el -> ( || ) (pred el)) False

-- | Computes logical implies. Assumes CNF.
implies :: Ord p => Conjunction p -> Conjunction p -> Bool
implies a b = 
    let subset = Set.isSubsetOf in
    -- Forall b
    forall (\elB -> exists (subset elB) a) b
    --forall (\elB -> exists (\elA -> subset elB elA)) b

--    Set.foldl' (\acc elB ->
--        let exists = Set.foldl' (\acc elA ->
--                (elA ?? elB) || acc
--              ) False a
--        in
--        exists && acc
--      ) True b
    

    
