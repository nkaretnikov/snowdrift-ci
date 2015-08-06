module Main where

import           Test.Tasty
import qualified Test.Parser as Parser
import qualified Test.Server as Server

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ Parser.tests
    , Server.tests
    ]
