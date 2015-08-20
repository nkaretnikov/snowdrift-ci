{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Snowdrift.CI.Process
    ( ExitCode
    , Stderr
    , Stdout
    , isExitFailure
    , foldActions
    , readProcessWithExitCode
    , verboseReadProcessWithExitCode
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.List
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Exit
import qualified System.Process as P

import           Snowdrift.CI.Printer
import           Snowdrift.CI.Type

readProcessWithExitCode :: Command -> IO (ExitCode, Stdout, Stderr)
readProcessWithExitCode Command {..} = do
    (exit, out, err) <-
        P.readProcessWithExitCode
            (T.unpack commandExec)
            (T.unpack <$> commandArgs)
            ""
    return (exit, Stdout $ T.pack out, Stderr $ T.pack err)

verboseReadProcessWithExitCode :: Command -> IO (ExitCode, Stdout, Stderr)
verboseReadProcessWithExitCode command@Command {..} = do
    let cmd = commandExec <> " " <> T.unwords commandArgs <> "\n"
    T.putStr cmd
    res@(_, out, err) <- readProcessWithExitCode command
    printStdout out
    printStderr err
    return $ over _2 (Stdout . (cmd <>) . unStdout) res

isExitFailure :: ExitCode -> Bool
isExitFailure (ExitFailure _) = True
isExitFailure ExitSuccess     = False

foldActions :: [IO (ExitCode, Stdout, Stderr)] -> IO ([ExitCode], Text)
foldActions xs =
    sequence xs >>= return . foldl' f ([],"")
  where
    f (es, t) (e, Stdout out, Stderr err) =
        ( es <> [e]
        , T.concat [t, out, err] )
