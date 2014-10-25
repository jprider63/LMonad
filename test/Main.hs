module Main (main) where

import System.Exit (exitFailure)
import Test.HUnit

import qualified DCLabelTest

tests :: Test
tests = TestList [ DCLabelTest.tests]

main :: IO ()
main = do 
    Counts _ _ errC failC <- runTestTT $ tests
    if errC + failC == 0 then
        return ()
    else
        exitFailure
