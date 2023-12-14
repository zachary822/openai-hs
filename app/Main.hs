{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (runConduit, (.|))
import Data.Conduit.Binary (sinkHandle)
import Data.String
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Network.HTTP.Client.Conduit
import Network.HTTP.Conduit
import Network.HTTP.Simple
import OpenAI
import OpenAI.Types
import System.Environment
import System.IO

main :: IO ()
main = do
  onMissingFile (loadFile defaultConfig) mempty

  apikey :: ApiKey <- fromString <$> getEnv "API_KEY"

  manager <- newManagerSettings defaultManagerSettings

  d <- mkAudioRequest <$> TIO.getContents

  hPutStrLn stderr ("model: " <> T.unpack d.model <> "text length: " <> show (T.length d.input))

  req <-
    setRequestMethod "POST" . setApiKey apikey . setRequestBodyJSON d
      <$> parseRequest "https://api.openai.com/v1/audio/speech"

  runResourceT $ do
    resp <- http req manager
    runConduit $ responseBody resp .| sinkHandle stdout
