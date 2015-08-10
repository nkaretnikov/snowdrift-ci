{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snowdrift.CI.Parser where

import Data.Aeson
import Control.Monad

import           Snowdrift.CI.Type (MergeRequest (MergeRequest))
import qualified Snowdrift.CI.Type as CI

instance FromJSON MergeRequest where
    parseJSON (Object o) = do
        attrs         <- o      .: "object_attributes"
        targetBranch  <- attrs  .: "target_branch"
        sourceBranch  <- attrs  .: "source_branch"
        state         <- attrs  .: "state"
        source        <- attrs  .: "source"
        sourceUrl     <- source .: "ssh_url"
        target        <- attrs  .: "target"
        targetUrl     <- target .: "ssh_url"
        commit        <- attrs  .: "last_commit"
        commitId      <- commit .: "id"
        commitMessage <- commit .: "message"
        author        <- commit .: "author"
        authorName    <- author .: "name"
        authorEmail   <- author .: "email"
        if (state `elem` ["opened", "reopened" :: Value])
        then return $! MergeRequest
                 { CI.targetBranch  = targetBranch
                 , CI.targetUrl     = targetUrl
                 , CI.sourceBranch  = sourceBranch
                 , CI.sourceUrl     = sourceUrl
                 , CI.commitId      = commitId
                 , CI.commitMessage = commitMessage
                 , CI.authorName    = authorName
                 , CI.authorEmail   = authorEmail
                 }
        else mzero
    parseJSON _ = mzero
