{-# LANGUAGE OverloadedStrings #-}

module Test.Server where

import Data.Monoid
import Control.Concurrent
import Control.Monad
import Network.HTTP hiding (port)
import System.IO
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Server tests"
    [ mergeRequestOpened
    -- , mergeRequestClosed
    ]

mergeRequestOpened :: TestTree
mergeRequestOpened = testCase "merge request opened" $ do
    let port  = 8086 :: Int
        file  = "data/snowdrift_ci_test.json"
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
    output @?= unlines
        [ "git clone git@github.com:nkaretnikov/snowdrift-ci-test.git target"
        , ""
        , "Cloning into 'target'..."
        , ""
        , "git fetch git@github.com:nkaretnikov/snowdrift-ci-test.git readme:readme"
        , ""
        , "From github.com:nkaretnikov/snowdrift-ci-test"
        , " * [new branch]      readme     -> readme"
        , ""
        , "git checkout master"
        , "Your branch is up-to-date with 'origin/master'."
        , ""
        , "Already on 'master'"
        , ""
        , "git merge readme"
        , "Updating a0c3d8f..0df2127"
        , "Fast-forward"
        , " README.md | 1 +"
        , " 1 file changed, 1 insertion(+)"
        , " create mode 100644 README.md"
        , ""
        , ""
        , "stack test"
        , "snowdrift-ci-test-0.1.0.0: configure (test)"
        , "Configuring snowdrift-ci-test-0.1.0.0..."
        , "Warning: Packages using 'cabal-version: >= 1.10' must specify the"
        , "'default-language' field for each component (e.g. Haskell98 or Haskell2010)."
        , "If a component uses different languages in different modules then list the"
        , "other ones in the 'other-languages' field."
        , "snowdrift-ci-test-0.1.0.0: build (test)"
        , "Preprocessing test suite 'test' for snowdrift-ci-test-0.1.0.0..."
        , "[1 of 1] Compiling Main             ( tests/Test.hs, .stack-work/dist/x86_64-linux/Cabal-1.22.2.0/build/test/test-tmp/Main.o )"
        , "Linking .stack-work/dist/x86_64-linux/Cabal-1.22.2.0/build/test/test ..."
        , "snowdrift-ci-test-0.1.0.0: test (suite: test)"
        , "stdout"
        , ""
        , "stderr"
        , ""
        , "test succeeded"
        ]
