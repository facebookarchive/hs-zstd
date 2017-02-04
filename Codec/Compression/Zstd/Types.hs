-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in
-- the LICENSE file in the root directory of this source tree. An
-- additional grant of patent rights can be found in the PATENTS file
-- in the same directory.

{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Codec.Compression.Zstd.Types
-- Copyright   : (c) 2016-present, Facebook, Inc. All rights reserved.
--
-- License     : BSD3
-- Maintainer  : bryano@fb.com
-- Stability   : experimental
-- Portability : GHC
--
-- Types supporting zstd compression and decompression.

module Codec.Compression.Zstd.Types
    (
      Decompress(..)
    , Dict(..)
    , fromDict
    , mkDict
    ) where

import Control.DeepSeq (NFData(..))
import Data.ByteString (ByteString)

-- | The result of a decompression operation.
data Decompress =
    Skip
  -- ^ Either the compressed frame was empty, or it was compressed in
  -- streaming mode and so its size is not known.
  | Error String
  -- ^ An error occurred.
  | Decompress ByteString
  -- ^ The payload was successfully decompressed.
  deriving (Eq, Read, Show)

-- | Compression dictionary.
newtype Dict = Dict {
    fromDict :: ByteString
  } deriving (Eq, Ord)

-- | Smart constructor.
mkDict :: ByteString -> Dict
mkDict d = Dict d

instance Show Dict where
    showsPrec n (Dict d) r = showsPrec n d r

instance Read Dict where
    readsPrec n s = map (\(a,b) -> (Dict a, b)) (readsPrec n s)

instance NFData Dict where
    rnf (Dict d) = rnf d
