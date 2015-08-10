{-# LANGUAGE OverloadedStrings #-}

module Snowdrift.CI.Type where

import Data.Text

data MergeRequest = MergeRequest
    { targetBranch  :: !Text
    , targetUrl     :: !Text
    , sourceBranch  :: !Text
    , sourceUrl     :: !Text
    , commitId      :: !Text
    , commitMessage :: !Text
    , authorName    :: !Text
    , authorEmail   :: !Text
    } deriving (Eq, Show)
