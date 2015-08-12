{-# LANGUAGE OverloadedStrings #-}

module Snowdrift.CI.Type where

import Prelude hiding (FilePath)

import Data.Text

newtype MergeRequestId = MergeRequestId { unMergeRequestId :: Int  } deriving (Eq, Show)
newtype TargetId       = TargetId       { unTargetId       :: Int  } deriving (Eq, Show)
newtype Branch         = Branch         { unBranch         :: Text } deriving (Eq, Show)
newtype Url            = Url            { unUrl            :: Text } deriving (Eq, Show)
newtype CommitId       = CommitId       { unCommitId       :: Text } deriving (Eq, Show)
newtype CommitMessage  = CommitMessage  { unCommitMessage  :: Text } deriving (Eq, Show)
newtype AuthorName     = AuthorName     { unAuthorName     :: Text } deriving (Eq, Show)
newtype AuthorEmail    = AuthorEmail    { unAuthorEmail    :: Text } deriving (Eq, Show)

data MergeRequest = MergeRequest
    { mergeRequestId :: !MergeRequestId
    , targetId       :: !TargetId
    , targetBranch   :: !Branch
    , targetUrl      :: !Url
    , sourceBranch   :: !Branch
    , sourceUrl      :: !Url
    , commitId       :: !CommitId
    , commitMessage  :: !CommitMessage
    , authorName     :: !AuthorName
    , authorEmail    :: !AuthorEmail
    } deriving (Eq, Show)

newtype FilePath = FilePath { unFilePath :: Text } deriving (Eq, Show)

newtype Stdout = Stdout { unStdout :: Text } deriving (Eq, Show)
newtype Stderr = Stderr { unStderr :: Text } deriving (Eq, Show)

data Status = MergeFailed
            | TestFailed
            | TestSucceeded
            deriving (Eq, Show)
