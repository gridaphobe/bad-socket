{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | Support module for the POSIX writev system call.
module IOVec where

import Foreign.C.Types (CChar, CInt, CSize)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

import Foreign.C.Types (CInt(..))
import System.Posix.Types (CSsize(..))

#include <sys/types.h>
#include <sys/uio.h>

data IOVec = IOVec
    { iovBase :: !(Ptr CChar)
    , iovLen  :: !CSize
    }

instance Storable IOVec where
  sizeOf _    = (#const sizeof(struct iovec))
  alignment _ = alignment (undefined :: CInt)

  peek p = do
    base <- (#peek struct iovec, iov_base) p
    len  <- (#peek struct iovec, iov_len)  p
    return $ IOVec base len

  poke p iov = do
    (#poke struct iovec, iov_base) p (iovBase iov)
    (#poke struct iovec, iov_len)  p (iovLen  iov)

foreign import ccall unsafe "writev"
  c_writev :: CInt -> Ptr IOVec -> CInt -> IO CSsize
