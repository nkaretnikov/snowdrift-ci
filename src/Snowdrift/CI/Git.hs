{-# LANGUAGE OverloadedStrings #-}

module Snowdrift.CI.Git where

import Prelude hiding (FilePath)

import Data.Monoid
import Data.Text
import Control.Monad

import Snowdrift.CI.Process
import Snowdrift.CI.Type

git :: [Text] -> IO (ExitCode, Stdout, Stderr)
git = readProcessWithExitCode . Command "git"

git_ :: [Text] -> IO ()
git_ = void . git

verboseGit :: [Text] -> IO (ExitCode, Stdout, Stderr)
verboseGit = verboseReadProcessWithExitCode . Command "git"

verboseGit_ :: [Text] -> IO ()
verboseGit_ = void . verboseGit

clone :: Url -> FilePath -> [Text]
clone (Url repo) (FilePath dir) = ["clone", repo, dir]

checkout :: Branch -> [Text]
checkout (Branch branch) = ["checkout", branch]

checkoutB :: Branch -> [Text]
checkoutB (Branch branch) = ["checkout", "-b", branch]

branchM :: Branch -> Branch -> [Text]
branchM (Branch oldBranch) (Branch newBranch) =
    ["branch", "-M", oldBranch, newBranch]

merge :: Branch -> [Text]
merge (Branch branch) = ["merge", branch]

fetch :: Url -> Branch -> Branch -> [Text]
fetch (Url repo) (Branch remBranch) (Branch locBranch) =
    ["fetch", repo, remBranch <> ":" <> locBranch]
