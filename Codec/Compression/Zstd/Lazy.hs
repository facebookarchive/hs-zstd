-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in
-- the LICENSE file in the root directory of this source tree. An
-- additional grant of patent rights can be found in the PATENTS file
-- in the same directory.

{-# LANGUAGE MultiWayIf #-}

-- |
-- Module      : Codec.Compression.Zstd.Lazy
-- Copyright   : (c) 2016-present, Facebook, Inc. All rights reserved.
--
-- License     : BSD3
-- Maintainer  : bryano@fb.com
-- Stability   : experimental
-- Portability : GHC
--
-- Lazy compression and decompression support for zstd.  Under the
-- hood, these are implemented using the streaming APIs.

module Codec.Compression.Zstd.Lazy
    (
      compress
    , decompress
    , S.maxCLevel
    ) where

import Data.ByteString.Lazy.Internal as L
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import qualified Codec.Compression.Zstd.Streaming as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- | Compress a payload.  The input will be consumed lazily, and the
-- compressed result generated lazily.
--
-- /Note:/ if any error occurs, compression will fail part-way through
-- with a call to 'error'.
compress :: Int
         -- ^ Compression level. Must be >= 1 and <= 'S.maxCLevel'.
         -> ByteString
         -- ^ Payload to compress.  This will be consumed lazily.
         -> ByteString
compress level bs = lazy (S.compress level) bs

-- | Decompress a payload.  The input will be consumed lazily, and the
-- decompressed result generated lazily.
--
-- /Note:/ if any error occurs, decompression will fail part-way
-- through with a call to 'error'.
decompress :: ByteString -> ByteString
decompress bs = lazy S.decompress bs

lazy :: IO S.Result -> ByteString -> ByteString
lazy start b0 = unsafePerformIO (go b0 =<< start)
 where
  go _            (S.Error who what) = error (who ++ ": " ++ what)
  go bs           (S.Produce o k)    = do
    os <- unsafeInterleaveIO (go bs =<< k)
    return (L.chunk o os)
  go (Chunk c cs) (S.Consume f) = go cs =<< f c
  go empty        (S.Consume f) = go empty =<< f B.empty
  go Empty        (S.Done o)    = return (chunk o Empty)
  go input        state = error $
                          "unpossible! " ++
                          show (L.length input) ++ " bytes of input left, " ++
                          show state ++ " stream state"
