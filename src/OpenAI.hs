{-# LANGUAGE OverloadedStrings #-}

module OpenAI where

import Control.Monad.Trans.Class
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

audioRequest ::
  (MonadResource m) => AudioRequest -> ConduitT i ByteString (OpenAIT m) ()
audioRequest payload = do
  apikey <- lift ask
  let req =
        setRequestMethod "POST" . setRequestApiKey apikey . setRequestBodyJSON payload $
          "https://api.openai.com/v1/audio/speech"
  httpSource req getResponseBody

chatRequest ::
  (MonadThrow m, MonadResource m) =>
  ChatRequest ->
  ConduitT i ByteString (OpenAIT m) ()
chatRequest payload = do
  apikey <- lift ask
  let req =
        setRequestMethod "POST" . setRequestApiKey apikey . setRequestBodyJSON payload $
          "https://api.openai.com/v1/chat/completions"
  httpSource req getResponseBody
