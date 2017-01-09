-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in
-- the LICENSE file in the root directory of this source tree. An
-- additional grant of patent rights can be found in the PATENTS file
-- in the same directory.

{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Codec.Compression.Zstd.FFI
-- Copyright   : (c) 2016-present, Facebook, Inc. All rights reserved.
--
-- License     : BSD3
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types and functions that support the low-level FFI bindings.

module Codec.Compression.Zstd.FFI.Types
    (
      Buffer(..)
    , In
    , Out
    , CCtx
    , DCtx
    , CDict
    , DDict
    , peekPtr
    , pokePtr
    , peekSize
    , pokeSize
    , peekPos
    , pokePos
    ) where

#define ZSTD_STATIC_LINKING_ONLY
#include <zstd.h>

import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Storable
import GHC.Ptr (Ptr(..))

-- | An opaque compression context structure.
data CCtx
-- | An opaque decompression context structure.
data DCtx

-- | An opaque pre-digested compression dictionary structure.
data CDict
-- | An opaque pre-digested decompression dictionary structure.
data DDict

-- | A tag type to indicate that a 'Buffer' is used for tracking input.
data In
-- | A tag type to indicate that a 'Buffer' is used for tracking output.
data Out

-- | A streaming buffer type. The type parameter statically indicates
-- whether the buffer is used to track an input or output buffer.
data Buffer io = forall a. Buffer {
      -- | Pointer to the start of the buffer.  This can be set once
      -- by the caller, and read by the streaming function.
      bufPtr  :: {-# UNPACK #-} !(Ptr a)
      -- | Size of the buffer (in bytes).  This can be set once by the
      -- caller, and is read by the streaming function.
    , bufSize :: {-# UNPACK #-} !CSize
      -- | Current offset into the buffer (in bytes).  This must be
      -- initially set to zero by the caller, and is updated by the
      -- streaming function.
    , bufPos  :: {-# UNPACK #-} !CSize
    }

instance Storable (Buffer io) where
    sizeOf _     = #const sizeof(ZSTD_inBuffer)
    alignment _  = alignment (undefined :: CInt)

    peek p = do
      ptr <- (#peek ZSTD_inBuffer, src) p
      size <- (#peek ZSTD_inBuffer, size) p
      pos <- (#peek ZSTD_inBuffer, pos) p
      return (Buffer ptr size pos)

    poke p (Buffer ptr size pos) = do
      (#poke ZSTD_inBuffer, src) p ptr
      (#poke ZSTD_inBuffer, size) p size
      (#poke ZSTD_inBuffer, pos) p pos

-- | Read the 'bufPtr' value from a 'Buffer'.
peekPtr :: Ptr (Buffer io) -> IO CSize
peekPtr p = (#peek ZSTD_inBuffer, src) p

-- | Write to the 'bufPtr' value in a 'Buffer'.
pokePtr :: Ptr (Buffer io) -> Ptr a -> IO ()
pokePtr dst p = (#poke ZSTD_inBuffer, src) dst p

-- | Read the 'bufSize' value from a 'Buffer'.
peekSize :: Ptr (Buffer io) -> IO CSize
peekSize p = (#peek ZSTD_inBuffer, size) p

-- | Write to the 'bufSize' value in a 'Buffer'.
pokeSize :: Ptr (Buffer io) -> CSize -> IO ()
pokeSize dst s = (#poke ZSTD_inBuffer, size) dst s

-- | Read the 'bufPos' value from a 'Buffer'.
peekPos :: Ptr (Buffer io) -> IO CSize
peekPos p = (#peek ZSTD_inBuffer, pos) p

-- | Write to the 'bufPos' value in a 'Buffer'.
pokePos :: Ptr (Buffer io) -> CSize -> IO ()
pokePos dst s = (#poke ZSTD_inBuffer, pos) dst s
