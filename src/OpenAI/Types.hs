{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OpenAI.Types where

import Data.Aeson
import Data.String
import Data.Text
import Data.Time.Clock
import GHC.Generics

opts :: Options
opts =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_'
    , omitNothingFields = True
    }

newtype ApiKey = ApiKey {getApiKey :: Text}

instance Show ApiKey where
  show _ = "<redacted>"

instance IsString ApiKey where
  fromString = ApiKey . fromString

data ChatRequest = ChatRequest
  { model :: !Text
  , messages :: ![ChatMessage]
  , temperature :: !(Maybe Double)
  }
  deriving (Generic, Show)

instance ToJSON ChatRequest where
  toJSON = genericToJSON opts

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
  , content :: !Text
  }
  deriving (Generic, Show)

instance FromJSON ChatMessage

instance ToJSON ChatMessage

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
