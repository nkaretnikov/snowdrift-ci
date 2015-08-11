{-# LANGUAGE OverloadedStrings #-}

module Snowdrift.CI.Printer where

import Prelude hiding (putStrLn)

import Data.Text.IO

import Snowdrift.CI.Type

printStatus :: Status -> IO ()
printStatus MergeFailed   = putStrLn "merge failed"
printStatus TestFailed    = putStrLn "test failed"
printStatus TestSucceeded = putStrLn "test succeeded"

printStdout :: Stdout -> IO ()
printStdout = putStrLn . unStdout

printStderr :: Stderr -> IO ()
printStderr = putStrLn . unStderr
