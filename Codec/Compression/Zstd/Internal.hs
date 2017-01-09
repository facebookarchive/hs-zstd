-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in
-- the LICENSE file in the root directory of this source tree. An
-- additional grant of patent rights can be found in the PATENTS file
-- in the same directory.

-- |
-- Module      : Codec.Compression.Zstd.Internal
-- Copyright   : (c) 2016-present, Facebook, Inc. All rights reserved.
--
-- License     : BSD3
-- Maintainer  : bryano@fb.com
-- Stability   : experimental
-- Portability : GHC
--
-- A fast lossless compression algorithm, targeting real-time
-- compression scenarios at zlib-level and better compression ratios.

module Codec.Compression.Zstd.Internal
    (
      CCtx(..)
    , DCtx(..)
    , compressWith
    , decompressWith
    , decompressedSize
    , withCCtx
    , withDCtx
    , withDict
    , trainFromSamples
    , getDictID
    ) where

import Codec.Compression.Zstd.Types (Decompress(..), Dict(..))
import Control.Exception.Base (bracket)
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word, Word8)
import Foreign.C.Types (CInt, CSize)
import Foreign.Marshal.Array (withArray)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import System.IO.Unsafe (unsafePerformIO)
import qualified Codec.Compression.Zstd.FFI as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

compressWith
    :: String
    -> (Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> CInt -> IO CSize)
    -> Int
    -> ByteString
    -> IO ByteString
compressWith name compressor level (PS sfp off len)
  | level < 1 || level > C.maxCLevel
              = bail name "unsupported compression level"
  | otherwise =
  withForeignPtr sfp $ \sp -> do
    let src = sp `plusPtr` off
    maxSize <- C.compressBound src
    dfp <- B.mallocByteString (fromIntegral maxSize)
    withForeignPtr dfp $ \dst -> do
      csz <- compressor dst maxSize src (fromIntegral len) (fromIntegral level)
      handleError csz name $ do
        let size = fromIntegral csz
        if csz < 128 || csz >= maxSize `div` 2
        then return (PS dfp 0 size)
        else B.create size $ \p -> B.memcpy p dst size

-- | Return the decompressed size of a compressed payload, as stored
-- in the payload's header.
--
-- The returned value will be `Nothing` if it is either not known
-- (probably because the payload was compressed using a streaming
-- API), empty, or too large to fit in an 'Int'.
--
-- /Note:/ this value should not be trusted, as it can be controlled
-- by an attacker.
decompressedSize :: ByteString -> Maybe Int
decompressedSize (PS fp off len) =
  unsafePerformIO . withForeignPtr fp $ \ptr -> do
    sz <- C.getDecompressedSize (ptr `plusPtr` off) (fromIntegral len)
    return $ if sz == 0 || sz > fromIntegral (maxBound :: Int)
             then Nothing
             else Just (fromIntegral sz)

decompressWith :: (Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO CSize)
               -> ByteString
               -> IO Decompress
decompressWith decompressor (PS sfp off len) = do
  withForeignPtr sfp $ \sp -> do
    let src = sp `plusPtr` off
    dstSize <- C.getDecompressedSize src (fromIntegral len)
    if dstSize == 0
      then return Skip
      else if dstSize > fromIntegral (maxBound :: Int)
           then return (Error "invalid compressed payload size")
           else do
      dfp <- B.mallocByteString (fromIntegral dstSize)
      size <- withForeignPtr dfp $ \dst ->
        decompressor dst (fromIntegral dstSize) src (fromIntegral len)
      return $ if C.isError size
               then Error (C.getErrorName size)
               else Decompress (PS dfp 0 (fromIntegral size))

-- | Compression context.
newtype CCtx = CCtx { getCCtx :: Ptr C.CCtx }

-- | Allocate a compression context, run an action that may reuse the
-- context as many times as it needs, then free the context.
withCCtx :: (CCtx -> IO a) -> IO a
withCCtx act =
  bracket (fmap CCtx (C.checkAlloc "withCCtx" C.createCCtx))
          (C.freeCCtx . getCCtx) act

-- | Decompression context.
newtype DCtx = DCtx { getDCtx :: Ptr C.DCtx }

-- | Allocate a decompression context, run an action that may reuse the
-- context as many times as it needs, then free the context.
withDCtx :: (DCtx -> IO a) -> IO a
withDCtx act =
  bracket (fmap DCtx (C.checkAlloc "withDCtx" C.createDCtx))
          (C.freeDCtx . getDCtx) act

withDict :: Dict -> (Ptr dict -> CSize -> IO a) -> IO a
withDict (Dict (PS fp off len)) act =
  withForeignPtr fp $ \ptr -> act (ptr `plusPtr` off) (fromIntegral len)

-- | Create and train a compression dictionary from a collection of
-- samples.
--
-- To create a well-trained dictionary, here are some useful
-- guidelines to keep in mind:
--
-- * A reasonable dictionary size is in the region of 100 KB.  (Trying
--   to specify a dictionary size of less than a few hundred bytes will
--   probably fail.)
--
-- * To train the dictionary well, it is best to supply a few thousand
--   training samples.
--
-- * The combined size of all training samples should be 100 or more
--   times larger than the size of the dictionary.
trainFromSamples :: Int
                 -- ^ Maximum size of the compression dictionary to
                 -- create. The actual dictionary returned may be
                 -- smaller.
                 -> [ByteString]
                 -- ^ Samples to train with.
                 -> Either String Dict
trainFromSamples capacity samples = unsafePerformIO $
  withArray (map B.length samples) $ \sizes -> do
    dfp <- B.mallocByteString capacity
    let PS sfp _ _ = B.concat samples
    withForeignPtr dfp $ \dict ->
      withForeignPtr sfp $ \sampPtr -> do
        dsz <- C.trainFromBuffer
               dict (fromIntegral capacity)
               sampPtr (castPtr sizes) (fromIntegral (length samples))
        if C.isError dsz
          then return (Left (C.getErrorName dsz))
          else fmap (Right . Dict) $ do
            let size = fromIntegral dsz
            if size < 128 || size >= capacity `div` 2
            then return (PS dfp 0 size)
            else B.create size $ \p -> B.memcpy p dict size

-- | Return the identifier for the given dictionary, or 'Nothing' if
-- not a valid dictionary.
getDictID :: Dict -> Maybe Word
getDictID dict = unsafePerformIO $ do
  n <- withDict dict C.getDictID
  return $! if n == 0
            then Nothing
            else Just (fromIntegral n)

handleError :: CSize -> String -> IO a -> IO a
handleError sizeOrError func act
  | C.isError sizeOrError
              = bail func (C.getErrorName sizeOrError)
  | otherwise = act

bail :: String -> String -> a
bail func str = error $ "Codec.Compression.Zstd." ++ func ++ ": " ++ str
