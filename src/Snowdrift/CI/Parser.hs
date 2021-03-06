{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snowdrift.CI.Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T

import           Snowdrift.CI.Type (Command (..), MergeRequest (MergeRequest))
import qualified Snowdrift.CI.Type as CI

stripPrefix :: Text -> Text -> Text
stripPrefix p t =
    if p `T.isPrefixOf` t
    then fromJust $ T.stripPrefix p t
    else t

stripSSH :: Text -> Text
stripSSH = stripPrefix "ssh://"

instance FromJSON MergeRequest where
    parseJSON (Object o) = do
        attrs          <- o      .: "object_attributes"
        mergeRequestId <- attrs  .: "id"
        targetId       <- attrs  .: "target_project_id"
        targetBranch   <- attrs  .: "target_branch"
        sourceBranch   <- attrs  .: "source_branch"
        state          <- attrs  .: "state"
        source         <- attrs  .: "source"
        sourceUrl      <- source .: "ssh_url"
        target         <- attrs  .: "target"
        targetUrl      <- target .: "ssh_url"
        commit         <- attrs  .: "last_commit"
        commitId       <- commit .: "id"
        commitMessage  <- commit .: "message"
        author         <- commit .: "author"
        authorName     <- author .: "name"
        authorEmail    <- author .: "email"
        if (state `elem` ["opened", "reopened" :: Value])
        then return $! MergeRequest
                 { CI.mergeRequestId = CI.MergeRequestId mergeRequestId
                 , CI.targetId       = CI.TargetId targetId
                 , CI.targetBranch   = CI.Branch targetBranch
                 , CI.targetUrl      = CI.Url $ stripSSH targetUrl
                 , CI.sourceBranch   = CI.Branch sourceBranch
                 , CI.sourceUrl      = CI.Url $ stripSSH sourceUrl
                 , CI.commitId       = CI.CommitId commitId
                 , CI.commitMessage  = CI.CommitMessage commitMessage
                 , CI.authorName     = CI.AuthorName authorName
                 , CI.authorEmail    = CI.AuthorEmail authorEmail
                 }
        else mzero
    parseJSON _ = mzero

parseCommands :: Text -> [Command]
parseCommands t = catMaybes cs
  where
    ts = T.lines t
    cs = (parse . T.words) <$> ts
    parse []     = Nothing
    parse (w:ws) = Just $ Command w ws
