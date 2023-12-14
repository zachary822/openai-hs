{-# LANGUAGE OverloadedStrings #-}

module OpenAI where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit
import Network.HTTP.Simple
import OpenAI.Types

setRequestApiKey :: ApiKey -> Request -> Request
setRequestApiKey = setRequestBearerAuth . encodeUtf8 . getApiKey

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

audioRequest ::
  (MonadThrow m, MonadResource m) =>
  Manager ->
  AudioRequest ->
  OpenAIT m (Response (ConduitT i ByteString (OpenAIT m) ()))
audioRequest manager d = do
  apikey <- ask
  req <-
    setRequestMethod "POST" . setRequestApiKey apikey . setRequestBodyJSON d
      <$> parseRequest "https://api.openai.com/v1/audio/speech"

  http req manager
