{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Char
import qualified Data.Foldable as F
import           Network.Wai.Handler.Warp
import           System.Environment
import           Web.Scotty

import           Snowdrift.CI

main :: IO ()
main = do
    let usage = error "Usage: PORT"
    getArgs >>= \case
        [port] -> if all isNumber port
                  then handle $ read port
                  else usage
        _      -> usage

handle :: Port -> IO ()
handle port = scottyOpts (Options 0 $ setPort port defaultSettings) $ do
    post "" $ do
        bs <- body
        F.forM_ (decode bs :: Maybe MergeRequest) $ \mr ->
            liftIO $ printStatus =<< testMergeRequest mr
