-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in
-- the LICENSE file in the root directory of this source tree. An
-- additional grant of patent rights can be found in the PATENTS file
-- in the same directory.

{-# LANGUAGE MultiWayIf #-}

-- |
-- Module      : Codec.Compression.Zstd.Streaming
-- Copyright   : (c) 2016-present, Facebook, Inc. All rights reserved.
--
-- License     : BSD3
-- Maintainer  : bryano@fb.com
-- Stability   : experimental
-- Portability : GHC
--
-- Streaming compression and decompression support for zstd.

module Codec.Compression.Zstd.Streaming
    (
      Result(..)
    , compress
    , decompress
    , maxCLevel
    ) where

import Codec.Compression.Zstd.FFI hiding (compress, decompress)
import Codec.Compression.Zstd.FFI.Types (peekPos)
import qualified Data.ByteString as B
import Data.ByteString.Internal (ByteString(..), mallocByteString)
import Foreign.Marshal.Alloc (finalizerFree, malloc)
import Foreign.C.Types (CSize)
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Storable (poke)
import Foreign.Ptr (Ptr, plusPtr)
import Data.Word (Word8)

-- | The result of a streaming compression or decompression step.
data Result
  = Produce ByteString (IO Result)
    -- ^ A single frame of transformed data, and an action that when
    -- executed will yield the next step in the streaming operation.
    -- The action is ephemeral; you should discard it as soon as you
    -- use it.
  | Consume (ByteString -> IO Result)
    -- ^ Provide the function with more input for the streaming
    -- operation to continue.  This function is ephemeral. You should
    -- call it exactly once, and discard it immediately after you call
    -- it.
    --
    -- To signal the end of a stream of data, supply an 'B.empty'
    -- input.
  | Error String String
    -- ^ An error has occurred. If an error occurs, the streaming
    -- operation cannot continue.
  | Done ByteString
    -- ^ The streaming operation has ended.  This payload may be
    -- empty. If it is not, it must be written out.
    --
    -- A non-empty payload consists of a frame epilogue, possibly
    -- preceded by any data left over from the final streaming step.

instance Show Result where
    show (Produce bs _) = "Produce " ++ show bs ++ " _"
    show (Consume _)    = "Consume _"
    show (Error n d)    = "Error " ++ show n ++ " " ++ show d
    show (Done bs)      = "Done " ++ show bs

-- | Begin a streaming compression operation.
--
-- The initial result will be either an 'Error' or a 'Consume'.
compress :: Int
         -- ^ Compression level. Must be >= 1 and <= 'maxCLevel'.
         -> IO Result
compress level
  | level < 1 || level > maxCLevel =
    return (Error "compress" "unsupported compression level")
  | otherwise =
  streaming
  createCStream
  p_freeCStream
  outSize
  (\cs -> initCStream cs (fromIntegral level))
  compressStream
  finish
 where
  outSize = fromIntegral cstreamOutSize
  finish cfp obfp opos dfp = do
    let cptr = unsafeForeignPtrToPtr cfp
        obuf = unsafeForeignPtrToPtr obfp
    check "endStream" (endStream cptr obuf) $ \leftover -> do
      touchForeignPtr cfp
      touchForeignPtr obfp
      if | leftover == 0 -> do
             opos1 <- fromIntegral `fmap` peekPos obuf
             Done `fmap` shrink outSize dfp opos1
         | leftover > 0 -> do
             dfp1 <- mallocByteString (fromIntegral leftover)
             poke obuf (buffer (unsafeForeignPtrToPtr dfp1) leftover)
             touchForeignPtr obfp
             bs <- shrink outSize dfp opos
             return (Produce bs (finish cfp obfp 0 dfp1))

type ConsumeBlock ctx io = Ptr ctx -> Ptr (Buffer Out)
                         -> Ptr (Buffer In) -> IO CSize

type Finish ctx io = ForeignPtr ctx -> ForeignPtr (Buffer Out)
                   -> Int -> ForeignPtr Word8 -> IO Result

streaming :: IO (Ptr ctx)
          -> FinalizerPtr ctx
          -> Int
          -> (Ptr ctx -> IO CSize)
          -> ConsumeBlock ctx io
          -> Finish ctx io
          -> IO Result
streaming createStream freeStream outSize initStream consumeBlock finish = do
  cx <- checkAlloc "createStream" createStream
  cxfp <- newForeignPtr freeStream cx
  check "initStream" (initStream cx) $ \_ -> do
    ibfp <- newForeignPtr finalizerFree =<< malloc
    obfp <- newForeignPtr finalizerFree =<< malloc
    dfp <- newOutput obfp
    advanceInput cxfp ibfp obfp 0 dfp
 where
  advanceInput cxfp ibfp obfp opos dfp = do
    let prompt (PS fp off len)
          | len == 0 = finish cxfp obfp opos dfp
          | otherwise = do
              withForeignPtr fp $ \sp0 ->
                withForeignPtr ibfp $ \ibuf ->
                  poke ibuf (buffer (sp0 `plusPtr` off) (fromIntegral len))
              consume cxfp ibfp 0 len obfp 0 dfp fp
    return (Consume prompt)
  newOutput obfp = do
    dfp <- mallocByteString outSize
    withForeignPtr dfp $ \dp ->
      withForeignPtr obfp $ \obuf ->
        poke obuf (buffer dp (fromIntegral outSize))
    return dfp
  consume cxfp ibfp ipos ilen obfp opos dfp fp = do
    if | fromIntegral ipos == ilen -> advanceInput cxfp ibfp obfp opos dfp
       | opos == outSize -> do
           let go = do
                 ndfp <- newOutput obfp
                 consume cxfp ibfp ipos ilen obfp 0 ndfp fp
           return (Produce (PS dfp 0 opos) go)
       | otherwise -> do
           let obuf = unsafeForeignPtrToPtr obfp
               ibuf = unsafeForeignPtrToPtr ibfp
           check "consumeBlock"
             (withForeignPtr cxfp $ \cptr ->
               consumeBlock cptr obuf ibuf <* touchForeignPtr fp) $ \_ -> do
             opos1 <- fromIntegral `fmap` peekPos obuf
             ipos1 <- peekPos ibuf
             touchForeignPtr obfp
             touchForeignPtr ibfp
             consume cxfp ibfp ipos1 ilen obfp opos1 dfp fp

-- | Begin a streaming decompression operation.
--
-- The initial result will be either an 'Error' or a 'Consume'.
decompress :: IO Result
decompress =
  streaming
  createDStream
  p_freeDStream
  outSize
  initDStream
  decompressStream
  finish
 where
  outSize = fromIntegral dstreamOutSize
  finish _cxfp _obfp opos dfp = Done `fmap` shrink outSize dfp opos

shrink :: Int -> ForeignPtr Word8 -> Int -> IO B.ByteString
shrink capacity dfp opos
  | opos == 0  = return B.empty
  | let unused = capacity - opos
    in unused >= 1024 || unused > capacity `rem` 8
               = return (B.copy (PS dfp 0 opos))
  | otherwise  = return (PS dfp 0 opos)

buffer :: Ptr a -> CSize -> Buffer io
buffer ptr size = Buffer ptr size 0

check :: String -> IO CSize -> (CSize -> IO Result) -> IO Result
check name act onSuccess = do
  ret <- act
  if isError ret
    then return (Error name (getErrorName ret))
    else onSuccess ret
