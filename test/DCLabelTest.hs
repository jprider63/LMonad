module DCLabelTest (tests) where

import qualified Data.Set as Set
import Test.HUnit

import LMonad
import LMonad.Label.DisjunctionCategory

b :: DCLabel Int
b = bottom

l :: DCLabel Int
l = DCLabel (Set.fromList [Set.singleton 1, Set.singleton 2]) (Set.singleton (Set.fromList [1,2]))

l' :: DCLabel Int
l' = DCLabel (Set.singleton (Set.fromList [1,2])) (Set.fromList [Set.singleton 1, Set.singleton 2])

l1 :: DCLabel Int
l1 = dcConfidentialitySingleton 4

l2 :: DCLabel Int
l2 = dcIntegritySingleton 4

joinTest :: Test
joinTest = TestCase $ do
    assertEqual "bottom `lub` l /= l" l $ b `lub` l
    assertEqual "bottom `lub` l /= l" l $ l `lub` b

    assertEqual "(dcConfidentialitySingleton 4) `lub` (dcIntegritySingleton 4) /= dcSingleton 4" (dcSingleton 4) $ l1 `lub` l2

    assertEqual "(dcSingleton 1) `lub` (dcSingleton 2) /= l" l $ (dcSingleton 1) `lub` (dcSingleton 2)

    assertEqual "l `lub` l /= l" l $ l `lub` l

meetTest :: Test
meetTest = TestCase $ do
    assertEqual "bottom `glb` l /= bottom" b $ l `glb` b
    assertEqual "bottom `glb` l /= bottom" b $ b `glb` l

    assertEqual "(dcConfidentialitySingleton 4) `glb` (dcIntegritySingleton 4) /= bottom" b $ l1 `glb` l2

    assertEqual "(dcSingleton 1) `glb` (dcSingleton 2) /= l'" l' $ (dcSingleton 1) `glb` (dcSingleton 2)

    assertEqual "l `glb` l /= l" l $ l `glb` l

tests :: Test
tests = TestLabel "DCLabel tests" $ TestList [
       TestLabel "Test join" joinTest
     , TestLabel "Test meet" meetTest
--      , TestLabel "Test canFlowTo" canFlowToTest
    ]
