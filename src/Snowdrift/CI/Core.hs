{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Snowdrift.CI.Core where

import           Prelude hiding (FilePath)

import           Data.Monoid
import qualified Data.Text as T
import           System.Directory
import           System.IO.Temp

import           Snowdrift.CI.Git
import           Snowdrift.CI.Stack
import           Snowdrift.CI.Process
import           Snowdrift.CI.Type

(</>) :: FilePath -> FilePath -> FilePath
x </> y = FilePath $ (unFilePath x) <> "/" <> (unFilePath y)

cd :: FilePath -> IO ()
cd = setCurrentDirectory . T.unpack . unFilePath

testMergeRequest :: MergeRequest -> IO Status
testMergeRequest MergeRequest {..} = do
    initDir <- FilePath . T.pack <$> getCurrentDirectory
    withSystemTempDirectory "snowdrift-ci" $ \tmp -> do
        let tmpDir    = FilePath $ T.pack tmp
            targetDir = FilePath "target"
        cd tmpDir
        verboseGit_ $ clone targetUrl targetDir
        cd $ tmpDir </> targetDir
        verboseGit_ $ fetch sourceUrl sourceBranch sourceBranch
        verboseGit_ $ checkout targetBranch
        (mexit, _, _) <- verboseGit $ merge sourceBranch
        if isExitFailure mexit
        then do
            cd initDir
            return MergeFailed
        else do
            (texit, _, _) <- verboseStack test
            cd initDir
            if isExitFailure texit
            then return TestFailed
            else return TestSucceeded
