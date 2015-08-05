{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Bake.Parser where

import Data.Aeson
import Control.Monad

import           Bake.Type (MergeRequest (MergeRequest))
import qualified Bake.Type as Bake

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
                 { Bake.targetBranch  = targetBranch
                 , Bake.targetUrl     = targetUrl
                 , Bake.sourceBranch  = sourceBranch
                 , Bake.sourceUrl     = sourceUrl
                 , Bake.commitId      = commitId
                 , Bake.commitMessage = commitMessage
                 , Bake.authorName    = authorName
                 , Bake.authorEmail   = authorEmail
                 }
        else mzero
    parseJSON _ = mzero
