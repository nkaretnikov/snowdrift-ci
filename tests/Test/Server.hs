{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Server where

import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Monoid
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Encoding (encodeUtf8)
import           Network.Wai.Handler.Warp
import           Network.Wreq
import qualified Network.Wreq as W
import           System.IO
import           System.Process
import           Test.Tasty
import           Test.Tasty.HUnit
import           Web.Scotty hiding (body, file)
import qualified Web.Scotty as S

tests :: TestTree
tests = testGroup "Server tests"
    [ mergeRequestOpened
    -- , mergeRequestClosed
    ]

withHeader :: Text -> Text -> ActionM ()
withHeader name value =
    S.header name >>= \case
        Nothing -> error $ T.unpack $ "no " <> name <> " header"
        Just h  -> when (h /= value) $
                       error $ T.unpack $ "invalid " <> name <> ": " <> h

mergeRequestOpened :: TestTree
mergeRequestOpened = testCase "merge request opened" $ do
    let port  = 8086 :: Int
        port' = succ port
        file  = "data/snowdrift_ci_test.json"
        token = "secret"
        stack = proc "stack" [ "exec", "snowdrift-ci"
                             , show port
                             , "http://localhost:" <> show port'
                             , token ]
        create = createProcess stack
                     { create_group = True
                     , std_out = CreatePipe }
        interrupt = traverseOf _4 interruptProcessGroupOf
    bracket create interrupt $ \(_, Just oh, _, ph) -> do
        let contentType   = "application/json; charset=utf-8"
            testSucceeded = "test succeeded"
        void $ forkIO $ scottyOpts (Options 0 $ setPort port' defaultSettings) $
            S.post "" $ do
                withHeader "Content-Type" contentType
                withHeader "PRIVATE-TOKEN" $ T.pack token
                body <- S.body
                let result' = "{\"note\":\"" <> unlines result <> testSucceeded <> "\"}"
                    -- This is just a hacky way to make '\n' and '\\n' match.
                    body'   = map chr <$> intercalate [10] $ splitOn [92,110] $ map ord $ LC.unpack body
                liftIO $ body' @?= result'

        str <- C.readFile file
        threadDelay 2000000
        let opts = defaults
                & W.header "Content-Type"
                .~ [encodeUtf8 $ T.toStrict contentType]
        void $ postWith opts ("http://localhost:" <> show port) str

        output <- hGetContents oh
        interruptProcessGroupOf ph
        output @?= unlines (result <> [testSucceeded, "200"])
  where
    result =
      [ "git clone git@github.com:nkaretnikov/snowdrift-ci-test.git target"
      , "Cloning into 'target'..."
      , "git fetch git@github.com:nkaretnikov/snowdrift-ci-test.git readme:readme"
      , "From github.com:nkaretnikov/snowdrift-ci-test"
      , " * [new branch]      readme     -> readme"
      , "git checkout master"
      , "Your branch is up-to-date with 'origin/master'."
      , "Already on 'master'"
      , "git merge readme"
      , "Updating a0c3d8f..0df2127"
      , "Fast-forward"
      , " README.md | 1 +"
      , " 1 file changed, 1 insertion(+)"
      , " create mode 100644 README.md"
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
      , "stderr"
      ]
