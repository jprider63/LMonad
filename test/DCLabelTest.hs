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

l3 :: DCLabel Int
l3 = dcSingleton 4

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

canFlowToTest :: Test
canFlowToTest = TestCase $ do
    assertBool "bottom `canFlowTo` l1" $ b `canFlowTo` l1
    assertBool "not (l1 `canFlowTo` bottom)" $ not $ l1 `canFlowTo` b

    assertBool "bottom `canFlowTo` l2" $ b `canFlowTo` l2
    assertBool "not (l2 `canFlowTo` bottom)" $ not $ l2 `canFlowTo` b

    assertBool "bottom `canFlowTo` l" $ b `canFlowTo` l
    assertBool "not (l `canFlowTo` bottom)" $ not $ l `canFlowTo` b

    assertBool "bottom `canFlowTo` l'" $ b `canFlowTo` l'
    assertBool "not (l' `canFlowTo` bottom)" $ not $ l' `canFlowTo` b

    assertBool "l1 `canFlowTo l3" $ l1 `canFlowTo` l3
    assertBool "not (l3 `canFlowTo l1)" $ not $ l3 `canFlowTo` l1

    assertBool "l2 `canFlowTo l3" $ l1 `canFlowTo` l3
    assertBool "not (l2 `canFlowTo l1)" $ not $ l3 `canFlowTo` l1

tests :: Test
tests = TestLabel "DCLabel tests" $ TestList [
       TestLabel "Test join" joinTest
     , TestLabel "Test meet" meetTest
     , TestLabel "Test canFlowTo" canFlowToTest
    ]
