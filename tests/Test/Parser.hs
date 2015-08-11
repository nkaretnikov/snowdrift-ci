{-# LANGUAGE OverloadedStrings #-}

module Test.Parser where

import Data.Aeson
import Data.ByteString.Lazy.Char8 as C
import Test.Tasty
import Test.Tasty.HUnit

import Snowdrift.CI

tests :: TestTree
tests = testGroup "Parser tests"
    [ mergeRequestOpened
    , mergeRequestClosed
    ]

decodeMergeRequest :: ByteString -> Maybe MergeRequest
decodeMergeRequest = decode

mergeRequestOpened :: TestTree
mergeRequestOpened = testCase "merge request opened" $ do
    str <- C.readFile "data/merge_request_opened.json"
    decodeMergeRequest str @?=
        Just (MergeRequest
            { targetBranch  = Branch "master"
            , targetUrl     = Url "git@example.com/awesome_target/awesome_project.git"
            , sourceBranch  = Branch "ms-viewport"
            , sourceUrl     = Url "git@example.com/awesome_source/awesome_project.git"
            , commitId      = CommitId "da1560886d4f094c3e6c9ef40349f7d38b5d27d7"
            , commitMessage = CommitMessage "fixed readme"
            , authorName    = AuthorName "GitLab dev user"
            , authorEmail   = AuthorEmail "gitlabdev@dv6700.(none)"
            })

mergeRequestClosed :: TestTree
mergeRequestClosed = testCase "merge request closed" $ do
    str <- C.readFile "data/merge_request_closed.json"
    decodeMergeRequest str @?= Nothing
