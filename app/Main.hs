{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (runConduit, (.|))
import Data.Conduit.Binary (sinkHandle)
import Data.String
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

  let d = mkAudioRequest "this is a test!"

  req <-
    setRequestMethod "POST" . setApiKey apikey . setRequestBodyJSON d
      <$> parseRequest "https://api.openai.com/v1/audio/speech"

  runResourceT $ do
    resp <- http req manager
    runConduit $ responseBody resp .| sinkHandle stdout
