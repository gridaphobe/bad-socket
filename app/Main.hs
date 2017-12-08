module Main where

import Control.Concurrent.Async
import Control.Monad (void)
import Network.Simple.TCP

import qualified Data.ByteString.Lazy as BL

import qualified TCP

main :: IO ()
main = do
  let listenPort = "9876"
      connectHost = "localhost"
      connectPort = "6789"
  serve HostAny listenPort $ \client ->
    connect connectHost connectPort $ \server -> do
      void $ concurrently (proxy server client "s->c")
                          (proxy client server "c->s")
  where
    proxy (s1, a1) (s2, a2) str = do
      payload <- recv s1 4096
      case payload of
        Nothing -> return ()
        Just p -> do
          TCP.sendAll s2 str $ BL.fromStrict p
          proxy (s1, a1) (s2, a2) str
