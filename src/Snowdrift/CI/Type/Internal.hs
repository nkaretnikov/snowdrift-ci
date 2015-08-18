{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snowdrift.CI.Type.Internal where

import Data.Aeson
import Data.Monoid
import qualified Data.Text as T

import Snowdrift.CI.Type
import Snowdrift.CI.Printer

-- | Render as a block of code.
toMarkdown :: String -> String
toMarkdown = ("```\n" <>)

-- http://doc.gitlab.com/ce/api/merge_requests.html#post-comment-to-mr
instance ToJSON Report where
    toJSON Report {..} = object ["note" .= note]
      where
        note = (T.pack $ toMarkdown $ T.unpack reportLog)
            <> pprintStatus reportStatus
