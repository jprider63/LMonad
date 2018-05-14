-- Some work derived from [LIO](http://hackage.haskell.org/package/lio-eci11), copyrighted under GPL.
--
-- Modifications by James Parker in 2014.

module LMonad.Label.DisjunctionCategory (
      Disjunction
    , Conjunction
    , DCLabel(..)
    , dcPublic
    , dcSingleton
    , dcConfidentialitySingleton
    , dcIntegritySingleton
    ) where

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

    deriving (Eq, Show)

dcPublic :: Ord p => DCLabel p
dcPublic = DCLabel Set.empty Set.empty

-- | Convenience function to convert a principal to confidentiality and integrity DCLabel.
dcSingleton :: Ord p => p -> DCLabel p
dcSingleton p = 
    let conj = Set.singleton (Set.singleton p) in
    DCLabel conj conj

-- | Convenience function to convert a principal to confidentiality DCLabel.
dcConfidentialitySingleton :: Ord p => p -> DCLabel p
dcConfidentialitySingleton p = DCLabel (Set.singleton (Set.singleton p)) $ Set.singleton Set.empty

-- | Convenience function to convert a principal to integrity DCLabel.
dcIntegritySingleton :: Ord p => p -> DCLabel p
dcIntegritySingleton p = DCLabel Set.empty (Set.singleton (Set.singleton p))

-- O(n)
forall :: (a -> Bool) -> Set a -> Bool
forall pred = Set.foldl' (\acc el -> acc && (pred el)) True

-- O(n)
exists :: (a -> Bool) -> Set a -> Bool
exists pred = Set.foldl' (\acc el -> acc || (pred el)) False

-- Equivalent to implies for disjunctions. O(n + m)
-- TODO: Add bloom filters here??? Would reduce to O(1) for most lookups. 
subset :: Ord p => Disjunction p -> Disjunction p -> Bool
subset = Set.isSubsetOf

-- | Computes logical implies. Assumes CNF. O(n * m * (n' + m'))
implies :: Ord p => Conjunction p -> Conjunction p -> Bool
implies a b = 
    -- forall b. exists a. a is a subset of b
    forall (\elB -> exists (\elA -> subset elA elB) a) b

-- O(n * (n' + m))
conjunctionInsertDisjunction :: Ord p => Conjunction p -> Disjunction p -> Conjunction p
conjunctionInsertDisjunction conj disj =
    -- Check if any element in acc implies disjunction.
    if exists (`subset` disj) conj then
        conj
    else
        -- Remove any elements in acc where that element is implied by the disjunction, and insert disjunction.
        Set.insert disj $ Set.filter (\el -> not (disj `subset` el)) conj

-- O(m * n * (n' + m')) ??
conjunctionAnd :: Ord p => Conjunction p -> Conjunction p -> Conjunction p
conjunctionAnd = Set.foldl' conjunctionInsertDisjunction

-- O(m * n * ()) ???
-- TODO: Optimize this more??? XXX
conjunctionOr :: Ord p => Conjunction p -> Conjunction p -> Conjunction p
conjunctionOr conj1 = Set.foldl' (\acc disj2 -> Set.foldl' (\acc disj1 ->
        conjunctionInsertDisjunction acc (Set.union disj1 disj2)
    ) acc conj1) Set.empty 

instance Ord p => Label (DCLabel p) where
    -- Meet
    glb (DCLabel c1 i1) (DCLabel c2 i2) = 
        DCLabel (c1 `conjunctionOr` c2) (i1 `conjunctionAnd` i2)

    -- Join
    lub (DCLabel c1 i1) (DCLabel c2 i2) = 
        DCLabel (c1 `conjunctionAnd` c2) (i1 `conjunctionOr` i2)

    -- Flow to
    canFlowTo (DCLabel c1 i1) (DCLabel c2 i2) = 
        c2 `implies` c1 && i1 `implies` i2

    -- Bottom
    bottom = DCLabel Set.empty $ Set.singleton Set.empty

instance Ord p => ToLabel Lattice (DCLabel p) where
    toConfidentialityLabel Top = DCLabel (Set.singleton Set.empty) Set.empty
    toConfidentialityLabel Bottom = bottom

    toIntegrityLabel Top = dcPublic
    toIntegrityLabel Bottom = bottom

