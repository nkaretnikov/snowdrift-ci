{-# LANGUAGE OverloadedStrings #-}

module Snowdrift.CI.Printer where

import           Prelude hiding (putStrLn)

import           Data.Text
import qualified Data.Text.IO as T

import           Snowdrift.CI.Type

pprintStatus :: Status -> Text
pprintStatus MergeFailed   = "merge failed"
pprintStatus TestFailed    = "test failed"
pprintStatus TestSucceeded = "test succeeded"

putStrLn :: Text -> IO ()
putStrLn "" = return ()
putStrLn t  = T.putStrLn $ dropWhileEnd (== '\n') t

printStatus :: Status -> IO ()
printStatus = putStrLn . pprintStatus

printStdout :: Stdout -> IO ()
printStdout = putStrLn . unStdout

printStderr :: Stderr -> IO ()
printStderr = putStrLn . unStderr
