{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snowdrift.CI.Type.Internal where

import Data.Aeson

import Snowdrift.CI.Type
import Snowdrift.CI.Printer

-- http://doc.gitlab.com/ce/api/merge_requests.html#post-comment-to-mr
instance ToJSON Status where
    toJSON s = object ["note" .= pprintStatus s]
