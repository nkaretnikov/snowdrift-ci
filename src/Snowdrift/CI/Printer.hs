{-# LANGUAGE OverloadedStrings #-}

module Snowdrift.CI.Printer where

import           Data.Text (Text)
import qualified Data.Text.IO as T

import           Snowdrift.CI.Type

pprintStatus :: Status -> Text
pprintStatus MergeFailed   = "merge failed"
pprintStatus TestFailed    = "test failed"
pprintStatus TestSucceeded = "test succeeded"

printStatus :: Status -> IO ()
printStatus = T.putStrLn . pprintStatus

printStdout :: Stdout -> IO ()
printStdout = T.putStrLn . unStdout

printStderr :: Stderr -> IO ()
printStderr = T.putStrLn . unStderr
