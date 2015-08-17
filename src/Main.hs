{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Char
import qualified Data.Foldable as F
import qualified Data.Text as T
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
    let usage = error "Usage: PORT"
    getArgs >>= \case
        [port, url, token]
            -> if all isNumber port
               then handle
                        (Port $ read port)
                        (Url $ T.pack url)
                        (Token $ T.pack token)
               else usage
        _   -> usage

setPort :: Port -> Settings -> Settings
setPort port settings =
    Warp.setPort (unPort port) settings

postWith :: Wreq.Options -> Url -> ByteString -> IO (Response ByteString)
postWith opts url text =
    Wreq.postWith opts (T.unpack $ unUrl url) text

handle :: Port -> Url -> Token -> IO ()
handle port url token =
    scottyOpts (S.Options 0 $ setPort port defaultSettings) $
        post "" $ do
            bs <- body
            F.forM_ (decode bs :: Maybe MergeRequest) $ \mr ->
                liftIO $ do
                    report@Report {..} <- testMergeRequest mr
                    printStatus reportStatus
                    -- http://doc.gitlab.com/ce/api/
                    -- http://doc.gitlab.com/ce/api/merge_requests.html#post-comment-to-mr
                    let opts = defaults
                             & header "Content-Type"
                            .~ ["application/json; charset=utf-8"]
                             & header "PRIVATE-TOKEN"
                            .~ [encodeUtf8 $ unToken token]
                    res <- postWith opts url $ encode report
                    print $ res ^. responseStatus . statusCode
