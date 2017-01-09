-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in
-- the LICENSE file in the root directory of this source tree. An
-- additional grant of patent rights can be found in the PATENTS file
-- in the same directory.

{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables #-}

module Main (main) where

import Criterion.Main
import Codec.Compression.Zstd as Zstd
import Codec.Compression.Zstd.Efficient as Zstd
import qualified Data.ByteString as B
import Data.ByteString.Char8 (ByteString)

compress_no_ctx :: Int -> ByteString -> Int
compress_no_ctx count input0 = go 0 0 input0
  where
    go i !acc input
      | i >= count = acc
      | otherwise  = go (i+1) (n+acc) input
        where    n = B.length (Zstd.compress 3 input)

compress_ctx :: Int -> ByteString -> IO Int
compress_ctx count input0 = Zstd.withCCtx $ \ctx -> go ctx 0 0 input0
  where
    go ctx i !acc input
      | i >= count = return acc
      | otherwise  = do
        n <- B.length `fmap` Zstd.compressCCtx ctx 3 input
        go ctx (i+1) (n+acc) input

main :: IO ()
main =
  defaultMain [
      env (B.take 200 `fmap` B.readFile "zstd.cabal") $ \ ~input ->
      let count = 1000 in
      bgroup "context" [
        bench "yes" $ nfIO (compress_ctx count input)
      , bench "no" $ nf (compress_no_ctx count) input
      ]
    ]
