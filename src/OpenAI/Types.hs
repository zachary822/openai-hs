{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OpenAI.Types where

import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.Types as AT
import Data.String
import Data.Text
import Data.Time.Clock
import GHC.Generics

type OpenAIT = ReaderT ApiKey

opts :: Options
opts =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_'
    , omitNothingFields = True
    }

newtype ApiKey = ApiKey {getApiKey :: Text} deriving newtype (IsString)

instance Show ApiKey where
  show _ = "<redacted>"

data ChatRequest = ChatRequest
  { model :: !Text
  , messages :: ![ChatMessage]
  , temperature :: !(Maybe Double)
  , stream :: !Bool
  }
  deriving (Generic, Show)

instance ToJSON ChatRequest where
  toJSON = genericToJSON opts

mkChatRequest :: Text -> [ChatMessage] -> ChatRequest
mkChatRequest model messages = ChatRequest{temperature = Nothing, stream = False, ..}

data AudioRequest = AudioRequest
  { model :: !Text
  , input :: !Text
  , voice :: !Text
  , responseFormat :: !(Maybe Text)
  }
  deriving (Generic, Show)

instance ToJSON AudioRequest where
  toJSON = genericToJSON opts

mkAudioRequest :: Text -> AudioRequest
mkAudioRequest input =
  AudioRequest
    { model = "tts-1-hd"
    , voice = "nova"
    , responseFormat = Just "flac"
    , ..
    }

data ChatMessage = ChatMessage
  { role :: !Text
  , content :: !Message
  }
  deriving (Generic, Show)

instance FromJSON ChatMessage

instance ToJSON ChatMessage

data Message
  = TextMessage !Text
  | ImageMessage ![MessageContent]
  deriving (Generic, Show)

instance FromJSON Message where
  parseJSON (String v) = return $ TextMessage v
  parseJSON v@(Array _) = ImageMessage <$> parseJSON v
  parseJSON invalid = typeMismatch "Object" invalid

instance ToJSON Message where
  toJSON (TextMessage t) = String t
  toJSON (ImageMessage t) = toJSON t

data MessageContent
  = TextContent
      { contentType :: !Text
      , text :: !Text
      }
  | ImageContent
      { contentType :: !Text
      , imageUrl :: !URL
      }
  deriving (Generic, Show)

instance FromJSON MessageContent where
  parseJSON = withObject "MessageContent" $ \v -> do
    ct <- v .: "type"
    mtxt <- v .:? "text"

    case mtxt of
      Just txt -> return $ TextContent ct txt
      Nothing -> ImageContent ct <$> v .: "image_url"

instance ToJSON MessageContent where
  toJSON (TextContent t txt) = AT.object ["type" .= t, "text" .= txt]
  toJSON (ImageContent t url) = AT.object ["type" .= t, "image_url" .= toJSON url]

newtype URL = URL {url :: Text} deriving (Generic, Show)

instance IsString URL where
  fromString = URL . pack

instance FromJSON URL

instance ToJSON URL

data OpenAIResponse = ChatResponse
  { id :: !Text
  , object :: !Text
  , created :: !NominalDiffTime
  , model :: !Text
  , usage :: !Usage
  , choices :: ![Choice]
  }
  deriving (Generic, Show)

instance FromJSON OpenAIResponse

data Usage = Usage
  { completionTokens :: !Integer
  , promptTokens :: !Integer
  , totalTokens :: !Integer
  }
  deriving (Generic, Show)

instance FromJSON Usage where
  parseJSON = genericParseJSON opts

data Choice = Choice
  { message :: !ChatMessage
  , finishReason :: !Text
  , index :: !Integer
  }
  deriving (Generic, Show)

instance FromJSON Choice where
  parseJSON = genericParseJSON opts
