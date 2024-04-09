{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import Data.Conduit.Combinators qualified as C
import Data.String
import Data.Text.IO qualified as TIO
import OpenAI
import OpenAI.Types
import System.Environment

main :: IO ()
main = do
  onMissingFile (loadFile defaultConfig) mempty

  apikey :: ApiKey <- fromString <$> getEnv "API_KEY"

  d <- mkAudioRequest <$> TIO.getContents

  runResourceT . flip runReaderT apikey $ do
    runConduit $ audioRequest d .| C.stdout
