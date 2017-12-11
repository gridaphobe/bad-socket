module Main where

import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv)

import qualified Data.ByteString.Lazy as BL

import qualified TCP

main :: IO ()
main = do
    let host = "localhost"
        listenPort = "9876"
        connectPort = "6789"
    myserve host listenPort $ \client ->
      myconnect host connectPort $ \server -> do
        void $ forkIO $ proxy server client "s->c"
        proxy client server "c->s"
  where
    proxy (s1, a1) (s2, a2) str = do
      payload <- recv s1 4096
      TCP.sendAll s2 str $ BL.fromStrict payload
      proxy (s1, a1) (s2, a2) str

myhints :: AddrInfo
myhints = defaultHints { addrSocketType = Stream }

myserve :: HostName -> ServiceName -> ((Socket, SockAddr) -> IO ()) -> IO ()
myserve host port f = do
  (addr:_) <- getAddrInfo (Just myhints) (Just host) (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  bind sock (addrAddress addr)
  listen sock 1
  forever $ forkIO . f =<< accept sock

myconnect :: HostName -> ServiceName -> ((Socket, SockAddr) -> IO ()) -> IO ()
myconnect host port f = do
  (addr:_) <- getAddrInfo (Just myhints) (Just host) (Just port)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  f (sock, addrAddress addr)
