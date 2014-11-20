module LMonad.Label.PowerSet where

import Data.Set (Set)
import qualified Data.Set as Set
import Prelude

import LMonad

-- | Power set label made of all combinations of the principals. 
data Ord p => PSLabel p = PSLabel {
        psLabelConfidentiality :: Set p
      , psLabelIntegrity :: Set p
    }
        deriving Show

psSingleton :: Ord p => p -> PSLabel p
psSingleton p = 
    let p' = Set.singleton p in
    PSLabel p' p'

psConfidentialitySingleton :: Ord p => p -> PSLabel p
psConfidentialitySingleton p = 
    let p' = Set.singleton p in
    PSLabel p' Set.empty

psIntegritySingleton :: Ord p => p -> PSLabel p
psIntegritySingleton p =
    let p' = Set.singleton p in
    PSLabel Set.empty p'

instance Ord p => Label (PSLabel p) where
    -- Meet
    glb (PSLabel c1 i1) (PSLabel c2 i2) =   
        let c = Set.intersection c1 c2 in
        let i = Set.intersection i1 i2 in
        PSLabel c i

    -- Join
    lub (PSLabel c1 i1) (PSLabel c2 i2) =   
        let c = Set.union c1 c2 in
        let i = Set.union i1 i2 in
        PSLabel c i

    -- Flow to
    canFlowTo (PSLabel c1 i1) (PSLabel c2 i2) =
        (Set.isSubsetOf c1 c2) && (Set.isSubsetOf i1 i2)

    -- Bottom
    bottom = 
        PSLabel Set.empty Set.empty

-- | Type alias for labeled power sets.
type PSLabeled p = Labeled (PSLabel p)
