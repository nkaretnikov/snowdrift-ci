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
    [ mergeRequestOpened
    , mergeRequestClosed
    ]

unsafeReadFile :: String -> ByteString
unsafeReadFile = unsafePerformIO . ByteString.readFile

decodeMergeRequest :: ByteString -> Maybe MergeRequest
decodeMergeRequest = decode

mergeRequestOpened :: TestTree
mergeRequestOpened = testCase "merge request opened" $
    decodeMergeRequest (unsafeReadFile "data/merge_request_opened.json") @?=
        Just (MergeRequest
            { targetBranch  = "master"
            , targetUrl     = "ssh://git@example.com/awesome_target/awesome_project.git"
            , sourceBranch  = "ms-viewport"
            , sourceUrl     = "ssh://git@example.com/awesome_source/awesome_project.git"
            , commitId      = "da1560886d4f094c3e6c9ef40349f7d38b5d27d7"
            , commitMessage = "fixed readme"
            , authorName    = "GitLab dev user"
            , authorEmail   = "gitlabdev@dv6700.(none)"
            })

mergeRequestClosed :: TestTree
mergeRequestClosed = testCase "merge request closed" $
    decodeMergeRequest (unsafeReadFile "data/merge_request_closed.json") @?=
        Nothing
