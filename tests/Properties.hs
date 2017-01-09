-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in
-- the LICENSE file in the root directory of this source tree. An
-- additional grant of patent rights can be found in the PATENTS file
-- in the same directory.

{-# LANGUAGE OverloadedStrings #-}

module Properties
    (
      tests
    ) where

import Codec.Compression.Zstd
import qualified Codec.Compression.Zstd.Lazy as L
import qualified Codec.Compression.Zstd.Streaming as S
import Codec.Compression.Zstd.Streaming (Result(..))
import Data.Bits (xor)
import Data.ByteString (ByteString, pack, unpack)
import Data.Monoid ((<>))
import System.IO.Unsafe (unsafePerformIO)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import QuickCheckUtils

Right dict = trainFromSamples 400 (replicate 100 "noooooooooo")

t_rechunk cs bs = L.toStrict (rechunk cs bs) == bs

t_roundtrip (CLevel n) (NE s) = decompress (compress n s) == Decompress s

t_dict_roundtrip (CLevel n) (NE s) =
  decompressUsingDict dict (compressUsingDict dict n s) == Decompress s

t_lazy_roundtrip (CLevel n) cs s =
  L.decompress (L.compress n (rechunk cs s)) == L.fromStrict s

-- Two lazy representations of an input compress to the same result.
t_lazy_compress_equiv (CLevel n) = unsquare $ \cs ds s ->
  L.compress n (rechunk cs s) == L.compress n (rechunk ds s)

t_stream_lazy_compress (CLevel n) = unsquare $ \cs ds s ->
  L.fromChunks (stream (S.compress n) cs s) == L.compress n (rechunk ds s)

t_stream_roundtrip (CLevel n) cs s =
  (B.concat . stream S.decompress cs . L.toStrict . L.compress n . L.fromStrict) s == s

tests :: Test
tests = testGroup "properties" [
    testProperty "rechunk" t_rechunk
  , testProperty "roundtrip" t_roundtrip
  , testProperty "dict_roundtrip" t_dict_roundtrip
  , testProperty "lazy_roundtrip" t_lazy_roundtrip
  , testProperty "lazy_compress_equiv" t_lazy_compress_equiv
  , testProperty "stream_lazy_compress" t_stream_lazy_compress
  , testProperty "stream_roundtrip" t_stream_roundtrip
  ]
