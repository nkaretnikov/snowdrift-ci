{-# LANGUAGE OverloadedStrings #-}

module Snowdrift.CI.Stack where

import Control.Monad
import Data.Text

import Snowdrift.CI.Process

stack :: [Text] -> IO (ExitCode, Stdout, Stderr)
stack = readProcessWithExitCode "stack"

stack_ :: [Text] -> IO ()
stack_ = void . stack

verboseStack :: [Text] -> IO (ExitCode, Stdout, Stderr)
verboseStack = verboseReadProcessWithExitCode "stack"

verboseStack_ :: [Text] -> IO ()
verboseStack_ = void . verboseStack

build :: [Text]
build = ["build"]

test :: [Text]
test = ["test"]
