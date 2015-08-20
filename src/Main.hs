{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

import           Prelude hiding (readFile, FilePath)

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Char
import qualified Data.Foldable as F
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Encoding (encodeUtf8)
import           Network.Wai.Handler.Warp (Settings, defaultSettings)
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wreq (Response, defaults, header, responseStatus, statusCode)
import qualified Network.Wreq as Wreq
import           System.Environment
import           Web.Scotty (body, post, scottyOpts)
import qualified Web.Scotty as S

import           Snowdrift.CI

main :: IO ()
main = do
    let usage = error "Usage: PORT URL TOKEN FILE"
    getArgs >>= \case
        [port, url, token, file]
            -> if all isNumber port
               then handle
                        (Port $ read port)
                        (Url $ T.pack url)
                        (Token $ T.pack token)
                        (FilePath $ T.pack file)
               else usage
        _   -> usage

setPort :: Port -> Settings -> Settings
setPort port settings =
    Warp.setPort (unPort port) settings

postWith :: Wreq.Options -> Url -> ByteString -> IO (Response ByteString)
postWith opts url text =
    Wreq.postWith opts (T.unpack $ unUrl url) text

mkCommentUrl :: Url -> TargetId -> MergeRequestId -> Url
mkCommentUrl (Url u) (TargetId t) (MergeRequestId m) =
    Url $ T.concat
        [ u
        , "/api/v3/projects/"
        , T.pack $ show t
        , "/merge_request/"
        , T.pack $ show m
        , "/comments" ]

readFile :: FilePath -> IO Text
readFile = T.readFile . T.unpack . unFilePath

handle :: Port -> Url -> Token -> FilePath -> IO ()
handle port url token file =
    scottyOpts (S.Options 0 $ setPort port defaultSettings) $
        post "" $ do
            bs <- body
            F.forM_ (decode bs) $ \mr@MergeRequest {..} ->
                liftIO $ do
                    commands <- readFile file
                    report@Report {..} <- testMergeRequest mr commands
                    printStatus reportStatus
                    -- http://doc.gitlab.com/ce/api/
                    -- http://doc.gitlab.com/ce/api/merge_requests.html#post-comment-to-mr
                    let opts = defaults
                             & header "Content-Type"
                            .~ ["application/json; charset=utf-8"]
                             & header "PRIVATE-TOKEN"
                            .~ [encodeUtf8 $ unToken token]
                    res <- postWith
                               opts
                               (mkCommentUrl url targetId mergeRequestId)
                               (encode report)
                    print $ res ^. responseStatus . statusCode
