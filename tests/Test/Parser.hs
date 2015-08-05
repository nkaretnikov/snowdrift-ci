{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import Data.Aeson
import Data.ByteString.Lazy as ByteString
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.HUnit

import Bake

tests :: TestTree
tests = testGroup "Parser tests"
  [ mergeRequest  ]

mergeRequestJSON :: ByteString
mergeRequestJSON =
    unsafePerformIO $ ByteString.readFile "data/merge_request.json"

mergeRequest :: TestTree
mergeRequest = testCase "merge request" $
    decode mergeRequestJSON @?=
        Just (MergeRequest
            { Bake.targetBranch  = "master"
            , Bake.targetUrl     = "ssh://git@example.com/awesome_target/awesome_project.git"
            , Bake.sourceBranch  = "ms-viewport"
            , Bake.sourceUrl     = "ssh://git@example.com/awesome_source/awesome_project.git"
            , Bake.commitId      = "da1560886d4f094c3e6c9ef40349f7d38b5d27d7"
            , Bake.commitMessage = "fixed readme"
            , Bake.authorName    = "GitLab dev user"
            , Bake.authorEmail   = "gitlabdev@dv6700.(none)"
            })
