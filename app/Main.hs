module Main where

import Protolude
import PostgresWebsockets

import System.IO (BufferMode (..), hSetBuffering)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin  LineBuffering
  hSetBuffering stderr NoBuffering

  putStrLn $ ("postgres-websockets " :: Text)
               <> prettyVersion
               <> " / Connects websockets to PostgreSQL asynchronous notifications."

  conf <- loadConfig
  void $ serve conf