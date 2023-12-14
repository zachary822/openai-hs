{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Conduit (runConduit, (.|))
import Data.Conduit.Binary (sinkHandle)
import Data.String
import Data.Text.IO qualified as TIO
import Network.HTTP.Client.Conduit
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

  runResourceT . flip runReaderT apikey $ do
    resp <- audioRequest manager d
    runConduit $ responseBody resp .| sinkHandle stdout
