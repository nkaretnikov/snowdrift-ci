{-# LANGUAGE OverloadedStrings #-}

module Snowdrift.CI.Process
    ( ExitCode
    , Stderr
    , Stdout
    , isExitFailure
    , readProcessWithExitCode
    , verboseReadProcessWithExitCode
    ) where

import           Prelude hiding (unwords, putStr, putStrLn)

import           Control.Applicative
import           Control.Lens
import           Data.Monoid
import           Data.Text
import           Data.Text.IO
import           System.Exit
import qualified System.Process as P

import           Snowdrift.CI.Printer
import           Snowdrift.CI.Type

readProcessWithExitCode :: Text -> [Text] -> IO (ExitCode, Stdout, Stderr)
readProcessWithExitCode proc args = do
    (exit, out, err) <-
        P.readProcessWithExitCode
            (unpack proc)
            (unpack <$> args)
            ""
    return (exit, Stdout $ pack out, Stderr $ pack err)

verboseReadProcessWithExitCode :: Text -> [Text] -> IO (ExitCode, Stdout, Stderr)
verboseReadProcessWithExitCode proc args = do
    let cmd = proc <> " " <> unwords args <> "\n"
    putStr cmd
    res@(_, out, err) <- readProcessWithExitCode proc args
    printStdout out
    printStderr err
    return $ over _2 (Stdout . (cmd <>) . unStdout) res

isExitFailure :: ExitCode -> Bool
isExitFailure (ExitFailure _) = True
isExitFailure ExitSuccess     = False
