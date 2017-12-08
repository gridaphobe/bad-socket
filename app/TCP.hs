{-# LANGUAGE BangPatterns #-}
module TCP
    (
    -- * Send data to a socket
      send
    , sendAll
    ) where

import Control.Monad (liftM, when)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Int (Int64)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable(..))

import Network.Socket (Socket(..))
-- import Network.Socket.ByteString.IOVec (IOVec(IOVec))
-- import Network.Socket.ByteString.Internal (c_writev)
import Network.Socket.Internal

import IOVec

import Foreign.C.Error

-- -----------------------------------------------------------------------------
-- Sending

send :: Socket      -- ^ Connected socket
     -> String
     -> ByteString  -- ^ Data to send
     -> IO Int64    -- ^ Number of bytes sent
send sock@(MkSocket fd _ _ _ _) str s = do
  let cs  = take maxNumChunks (L.toChunks s)
      len = length cs
  liftM fromIntegral . allocaArray len $ \ptr ->
    withPokes cs ptr $ \niovs -> do
      print ("send["++str++"]: writing ", L.length s)
      r <- throwSocketErrorWaitWrite sock "writev" $ do
        r <- c_writev (fromIntegral fd) ptr niovs
        print ("writev["++str++"]: ", r)
        when (r == (-1)) $ do
          Errno err <- getErrno
          print ("errno["++str++"]: ", err, errnoToIOError "writev" (Errno err) Nothing Nothing)
        return r
      print ("send["++str++"]: wrote   ", r)
      return r
  where
    withPokes ss p f = loop ss p 0 0
      where loop (c:cs) q k !niovs
                | k < maxNumBytes =
                    unsafeUseAsCStringLen c $ \(ptr,len) -> do
                      poke q $ IOVec ptr (fromIntegral len)
                      loop cs (q `plusPtr` sizeOf (undefined :: IOVec))
                              (k + fromIntegral len) (niovs + 1)
                | otherwise = f niovs
            loop _ _ _ niovs = f niovs
    maxNumBytes  = 4194304 :: Int  -- maximum number of bytes to transmit in one system call
    maxNumChunks = 1024    :: Int  -- maximum number of chunks to transmit in one system call

sendAll :: Socket      -- ^ Connected socket
        -> String
        -> ByteString  -- ^ Data to send
        -> IO ()
sendAll sock str bs = do
  sent <- send sock str bs
  let bs' = L.drop sent bs
  unless (L.null bs') $ sendAll sock str bs'
