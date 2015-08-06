{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Char
import Network.Wai.Handler.Warp
import System.Environment
import Web.Scotty

import Bake

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
        liftIO $ print (decode bs :: Maybe MergeRequest)
