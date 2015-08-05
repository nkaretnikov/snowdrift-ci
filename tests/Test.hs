module Main where

import Test.Tasty
import qualified Test.Parser as Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Parser.tests ]
