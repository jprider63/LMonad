module LMonad.Label.PowerSet where

import Data.Set (Set)
import qualified Data.Set as Set
import Prelude

import LMonad.Label

-- | Power set label made of all combinations of the principals. 
data Ord p => PowerSetLabel p = PowerSetLabel {
        powerSetLabelConfidentiality :: Set p
      , powerSetLabelIntegrity :: Set p
    }
        deriving Show

instance Ord p => Label (PowerSetLabel p) where
    -- Meet
    glb (PowerSetLabel c1 i1) (PowerSetLabel c2 i2) =   
        let c = Set.intersection c1 c2 in
        let i = Set.intersection i1 i2 in
        PowerSetLabel c i

    -- Join
    lub (PowerSetLabel c1 i1) (PowerSetLabel c2 i2) =   
        let c = Set.union c1 c2 in
        let i = Set.union i1 i2 in
        PowerSetLabel c i

    -- canFlowTo
    canFlowTo (PowerSetLabel c1 i1) (PowerSetLabel c2 i2) =
        (Set.isSubsetOf c1 c2) && (Set.isSubsetOf i1 i2)

    -- Bottom
    bottom = 
        PowerSetLabel Set.empty Set.empty

-- | Type alias for labeled power sets.
type PowerSetLabeled p = Labeled PowerSetLabel p
