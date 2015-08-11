{-# LANGUAGE OverloadedStrings #-}

module Test.Server where

import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe
import           Data.Monoid
import           Control.Concurrent
import           Control.Monad
import           Network.HTTP hiding (port)
import           System.IO
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit

import           Test.Parser hiding (mergeRequestOpened, mergeRequestClosed)

tests :: TestTree
tests = testGroup "Server tests"
    [ mergeRequestOpened
    -- , mergeRequestClosed
    ]

mergeRequestOpened :: TestTree
mergeRequestOpened = testCase "merge request opened" $ do
    let port  = 8086 :: Int
        file  = "data/merge_request_opened.json"
        stack = proc "stack" ["exec", "snowdrift-ci", show port]
    (_, Just oh, _, ph) <- createProcess $ stack
                               { create_group = True
                               , std_out = CreatePipe
                               }
    str <- readFile file
    threadDelay 2000000
    void $ simpleHTTP $
        postRequestWithBody
            ("http://localhost:" <> show port)
            "application/json; charset=utf-8"
            str
    output <- hGetContents oh
    interruptProcessGroupOf ph
    let mr = fromJust $ decodeMergeRequest $ C.pack str
    output @?= ("Just (" <> show mr <> ")\n")
