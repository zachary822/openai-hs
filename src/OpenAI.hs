{-# LANGUAGE OverloadedStrings #-}

module OpenAI where

import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple
import OpenAI.Types

setApiKey :: ApiKey -> Request -> Request
setApiKey = setRequestBearerAuth . encodeUtf8 . getApiKey

fileTypes :: [(ByteString, String)]
fileTypes =
  [ ("audio/flac", "flac")
  , ("audio/aac", "aac")
  , ("audio/mpeg", "mp3")
  ]

getFileExt :: Response a -> Maybe String
getFileExt resp = lookup (head hs) fileTypes
 where
  hs = getResponseHeader "content-type" resp
