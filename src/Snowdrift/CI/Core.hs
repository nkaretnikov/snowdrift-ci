{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Snowdrift.CI.Core where

import           Prelude hiding (FilePath)

import           Control.Applicative
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           System.Directory
import           System.IO.Temp

import           Snowdrift.CI.Git
import           Snowdrift.CI.Parser
import           Snowdrift.CI.Process
import           Snowdrift.CI.Type

(</>) :: FilePath -> FilePath -> FilePath
x </> y = FilePath $ (unFilePath x) <> "/" <> (unFilePath y)

cd :: FilePath -> IO ()
cd = setCurrentDirectory . T.unpack . unFilePath

testMergeRequest :: MergeRequest -> Text -> IO Report
testMergeRequest MergeRequest {..} testActions = do
    initDir <- FilePath . T.pack <$> getCurrentDirectory
    withSystemTempDirectory "snowdrift-ci" $ \tmp -> do
        let tmpDir    = FilePath $ T.pack tmp
            targetDir = FilePath "target"
        cd tmpDir
        let mergeOutput    (_,y,z) = unStdout y <> unStderr z
            mergeOutput' t@(x,_,_) = (x, mergeOutput t)
        cloneLog <- mergeOutput <$> verboseGit (clone targetUrl targetDir)
        cd $ tmpDir </> targetDir
        fetchLog <- mergeOutput <$> verboseGit (fetch sourceUrl sourceBranch sourceBranch)
        checkoutLog <- mergeOutput <$> verboseGit (checkout targetBranch)
        (mexit, mergeLog) <- mergeOutput' <$> verboseGit (merge sourceBranch)
        let logs = T.concat [cloneLog, fetchLog, checkoutLog, mergeLog]
        if isExitFailure mexit
        then do
            cd initDir
            return $ Report MergeFailed logs
        else do
            (texits, testLog) <-
                foldActions $
                    verboseReadProcessWithExitCode <$>
                        parseCommands testActions
            cd initDir
            let logs' = logs <> testLog
            if isExitFailure `any` texits
            then return $ Report TestFailed    logs'
            else return $ Report TestSucceeded logs'
