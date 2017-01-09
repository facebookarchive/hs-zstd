-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in
-- the LICENSE file in the root directory of this source tree. An
-- additional grant of patent rights can be found in the PATENTS file
-- in the same directory.

-- |
-- Module      : Codec.Compression.Zstd.Base
-- Copyright   : (c) 2016-present, Facebook, Inc. All rights reserved.
--
-- License     : BSD3
-- Maintainer  : bryano@fb.com
-- Stability   : experimental
-- Portability : GHC
--
-- Mid-level bindings to the native zstd compression library.  These
-- bindings provide a little more safety and ease of use than the
-- lowest-level FFI bindings.  Unless you have highly specialized
-- needs, you should use the streaming or other higher-level APIs
-- instead.

module Codec.Compression.Zstd.Base
    (
    -- * One-shot functions
      compress
    , compressBound
    , maxCLevel
    , decompress
    , getDecompressedSize

    -- ** Cheaper operations using contexts
    -- *** Compression
    , CCtx
    , withCCtx
    , compressCCtx

    -- *** Decompression
    , DCtx
    , withDCtx
    , decompressDCtx

    -- * Streaming operations
    -- ** Streaming types
    , CStream
    , DStream
    , FFI.Buffer(..)
    , FFI.In
    , FFI.Out

    -- ** Streaming compression
    , cstreamInSize
    , cstreamOutSize
    , createCStream
    , initCStream
    , compressStream
    , endStream

    -- ** Streaming decompression
    , dstreamInSize
    , dstreamOutSize
    , createDStream
    , initDStream
    , decompressStream

    -- * Dictionary compression
    , trainFromBuffer
    , getDictID

    , compressUsingDict
    , decompressUsingDict

    -- ** Pre-digested dictionaries
    -- *** Compression
    , CDict
    , createCDict
    , compressUsingCDict

    -- *** Decompression
    , DDict
    , createDDict
    , decompressUsingDDict
    ) where

import Codec.Compression.Zstd.Base.Types (CDict(..), DDict(..))
import Codec.Compression.Zstd.FFI.Types (CCtx, DCtx)
import Control.Exception.Base (bracket)
import Data.Word (Word, Word64)
import Foreign.C.Types (CSize)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import qualified Codec.Compression.Zstd.FFI as FFI

-- | Compress bytes from source buffer into destination buffer.
-- The destination buffer must be already allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error description if it fails.
compress :: Ptr dst         -- ^ Destination buffer.
         -> Int             -- ^ Capacity of destination buffer.
         -> Ptr src         -- ^ Source buffer.
         -> Int             -- ^ Size of source buffer.
         -> Int             -- ^ Compression level.
         -> IO (Either String Int)
compress dst dstSize src srcSize level = checkError $
  FFI.compress dst (fromIntegral dstSize) src (fromIntegral srcSize)
               (fromIntegral level)

-- | The maximum compression level supported by the library.
maxCLevel :: Int
maxCLevel = fromIntegral FFI.maxCLevel

-- | Compute the maximum compressed size of given source buffer.
compressBound :: Ptr src         -- ^ Source buffer.
              -> IO Int
compressBound src = fromIntegral `fmap` FFI.compressBound src

-- | Decompress a buffer.  The destination buffer must be already
-- allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error description if it fails.
decompress :: Ptr dst         -- ^ Destination buffer.
           -> Int             -- ^ Capacity of destination buffer.
           -> Ptr src         -- ^ Source buffer.
           -> Int
           -- ^ Size of compressed input.  This must be exact, so
           -- for example supplying the size of a buffer that is
           -- larger than the compressed input will cause a failure.
           -> IO (Either String Int)
decompress dst dstSize src srcSize = checkError $
  FFI.decompress dst (fromIntegral dstSize) src (fromIntegral srcSize)

-- | Returns the decompressed size of a compressed payload if known.
--
-- To discover precisely why a result is not known, follow up with
-- 'FFI.getFrameParams'.
getDecompressedSize :: Ptr src
                    -> Int
                    -> IO (Maybe Word64)
getDecompressedSize src srcSize = do
  ret <- FFI.getDecompressedSize src (fromIntegral srcSize)
  return $! if ret == 0
            then Nothing
            else Just (fromIntegral ret)

-- | Allocate a compression context, run an action that may reuse the
-- context as many times as it needs, then free the context.
withCCtx :: (Ptr CCtx -> IO a) -> IO a
withCCtx act =
  bracket (FFI.checkAlloc "withCCtx" FFI.createCCtx) FFI.freeCCtx act

-- | Compress bytes from source buffer into destination buffer.
-- The destination buffer must be already allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error description if it fails.
compressCCtx :: Ptr CCtx    -- ^ Compression context.
             -> Ptr dst     -- ^ Destination buffer.
             -> Int         -- ^ Capacity of destination buffer.
             -> Ptr src     -- ^ Source buffer.
             -> CSize       -- ^ Size of source buffer.
             -> Int         -- ^ Compression level.
             -> IO (Either String Int)
compressCCtx cctx dstPtr dstSize srcPtr srcSize level = checkError $
  FFI.compressCCtx cctx dstPtr (fromIntegral dstSize)
                        srcPtr (fromIntegral srcSize)
                        (fromIntegral level)

-- | Allocate a decompression context, run an action that may reuse
-- the context as many times as it needs, then free the context.
withDCtx :: (Ptr DCtx -> IO a) -> IO a
withDCtx act =
  bracket (FFI.checkAlloc "withDCtx" FFI.createDCtx) FFI.freeDCtx act

-- | Decompress a buffer.  The destination buffer must be already
-- allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error description if it fails.
decompressDCtx :: Ptr DCtx        -- ^ Decompression context.
               -> Ptr dst         -- ^ Destination buffer.
               -> Int             -- ^ Capacity of destination buffer.
               -> Ptr src         -- ^ Source buffer.
               -> Int
               -- ^ Size of compressed input.  This must be exact, so
               -- for example supplying the size of a buffer that is
               -- larger than the compressed input will cause a failure.
               -> IO (Either String Int)
decompressDCtx dctx dst dstSize src srcSize = checkError $
  FFI.decompressDCtx dctx dst (fromIntegral dstSize) src (fromIntegral srcSize)

-- | Recommended size for input buffer.
cstreamInSize :: Int
cstreamInSize = fromIntegral FFI.cstreamInSize

-- | Recommended size for output buffer.
cstreamOutSize :: Int
cstreamOutSize = fromIntegral FFI.cstreamOutSize

-- | A context for streaming compression.
newtype CStream = CS (ForeignPtr FFI.CStream)

-- | Create a 'CStream' value.  After use, this will eventually be
-- freed via a finalizer.
createCStream :: IO CStream
createCStream = do
  cs <- FFI.checkAlloc "createCStream" FFI.createCStream
  cfp <- newForeignPtr FFI.p_freeCStream cs
  return (CS cfp)

-- | Begin a new streaming compression operation.
initCStream :: CStream
            -> Int              -- ^ Compression level.
            -> IO (Either String ())
initCStream (CS cfp) level =
  fmap (fmap (const ())) $ checkError $ withForeignPtr cfp $ \cs ->
    FFI.initCStream cs (fromIntegral level)

-- | Consume part or all of an input.
compressStream :: CStream -> Ptr (FFI.Buffer FFI.Out) -> Ptr (FFI.Buffer FFI.In)
               -> IO (Either String Int)
compressStream (CS cfp) bi bo = checkError $
  withForeignPtr cfp $ \cs ->
    FFI.compressStream cs bi bo

-- | End a compression stream. This performs a flush and writes a
-- frame epilogue.
endStream :: CStream -> Ptr (FFI.Buffer FFI.Out) -> IO (Either String Int)
endStream (CS cfp) bo = checkError $
  withForeignPtr cfp $ \cs ->
    FFI.endStream cs bo

-- | Recommended size for input buffer.
dstreamInSize :: Int
dstreamInSize = fromIntegral FFI.dstreamInSize

-- | Recommended size for output buffer.
dstreamOutSize :: Int
dstreamOutSize = fromIntegral FFI.dstreamOutSize

-- | A context for streaming decompression.
newtype DStream = DS (ForeignPtr FFI.DStream)

-- | Create a streaming decompression context.  After use, this will
-- eventually be freed via a finalizer.
createDStream :: IO DStream
createDStream = do
  ds <- FFI.checkAlloc "createDStream" FFI.createDStream
  dfp <- newForeignPtr FFI.p_freeDStream ds
  return (DS dfp)

-- | Begin a new streaming decompression operation.
initDStream :: DStream
            -> IO (Either String ())
initDStream (DS dfp) =
  fmap (fmap (const ())) $ checkError $ withForeignPtr dfp FFI.initDStream

-- | Consume part or all of an input.
decompressStream :: DStream -> Ptr (FFI.Buffer FFI.Out)
                 -> Ptr (FFI.Buffer FFI.In) -> IO (Either String Int)
decompressStream (DS dfp) bi bo = checkError $
  withForeignPtr dfp $ \ds ->
    FFI.decompressStream ds bi bo

-- | Train a dictionary from a collection of samples.
-- Returns the number size of the resulting dictionary.
trainFromBuffer :: Ptr dict
                -- ^ Preallocated dictionary buffer.
                -> Int
                -- ^ Capacity of dictionary buffer.
                -> Ptr samples
                -- ^ Concatenated samples.
                -> Ptr Int
                -- ^ Array of sizes of samples.
                -> Int
                -- ^ Number of samples.
                -> IO (Either String Int)
trainFromBuffer dictPtr dictSize sampPtr sampSizes sampCount = checkError $
  FFI.trainFromBuffer dictPtr (fromIntegral dictSize)
                      sampPtr (castPtr sampSizes) (fromIntegral sampCount)

-- | Return the identifier for the given dictionary, or 'Nothing' if
-- not a valid dictionary.
getDictID :: Ptr dict -> Int -> IO (Maybe Word)
getDictID ptr size = do
  n <- FFI.getDictID ptr (fromIntegral size)
  return $! if n == 0
            then Nothing
            else Just (fromIntegral n)

-- | Compress bytes from source buffer into destination buffer.
-- The destination buffer must be already allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error description if it fails.
compressUsingDict :: Ptr CCtx
                  -> Ptr dst         -- ^ Destination buffer.
                  -> Int             -- ^ Capacity of destination buffer.
                  -> Ptr src         -- ^ Source buffer.
                  -> Int             -- ^ Size of source buffer.
                  -> Ptr dict        -- ^ Dictionary.
                  -> Int             -- ^ Size of dictionary.
                  -> Int             -- ^ Compression level.
                  -> IO (Either String Int)
compressUsingDict ctx dst dstSize src srcSize dict dictSize level = checkError $
  FFI.compressUsingDict ctx dst (fromIntegral dstSize)
    src (fromIntegral srcSize) dict (fromIntegral dictSize) (fromIntegral level)

-- | Decompress a buffer.  The destination buffer must be already
-- allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error description if it fails.
decompressUsingDict :: Ptr DCtx
                    -> Ptr dst         -- ^ Destination buffer.
                    -> Int             -- ^ Capacity of destination buffer.
                    -> Ptr src         -- ^ Source buffer.
                    -> Int
                    -- ^ Size of compressed input.  This must be exact, so
                    -- for example supplying the size of a buffer that is
                    -- larger than the compressed input will cause a failure.
                    -> Ptr dict        -- ^ Dictionary.
                    -> Int             -- ^ Size of dictionary.
                    -> IO (Either String Int)
decompressUsingDict ctx dst dstSize src srcSize dict dictSize = checkError $
  FFI.decompressUsingDict ctx dst (fromIntegral dstSize)
    src (fromIntegral srcSize) dict (fromIntegral dictSize)

-- | Create a pre-digested compression dictionary.  After use, this
-- will eventually be freed via a finalizer.
createCDict :: Ptr dict
            -- ^ Dictionary.
            -> Int
            -- ^ Size of dictionary.
            -> Int
            -- ^ Compression level.
            -> IO CDict
createCDict dict size level = do
  cd <- FFI.checkAlloc "createCDict" $
        FFI.createCDict dict (fromIntegral size) (fromIntegral level)
  fp <- newForeignPtr FFI.p_freeCDict cd
  return (CD fp)

-- | Compress bytes from source buffer into destination buffer, using
-- a pre-digested dictionary.  The destination buffer must be already
-- allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error description if it fails.
compressUsingCDict
    :: Ptr CCtx    -- ^ Compression context.
    -> Ptr dst     -- ^ Destination buffer.
    -> Int         -- ^ Capacity of destination buffer.
    -> Ptr src     -- ^ Source buffer.
    -> Int         -- ^ Size of source buffer.
    -> CDict       -- ^ Dictionary.
    -> IO (Either String Int)
compressUsingCDict ctx dst dstSize src srcSize (CD fp) =
  checkError . withForeignPtr fp $ \ dict ->
  FFI.compressUsingCDict ctx dst (fromIntegral dstSize)
    src (fromIntegral srcSize) dict

-- | Create a pre-digested decompression dictionary.  After use, this
-- will eventually be freed via a finalizer.
createDDict :: Ptr dict
            -- ^ Dictionary.
            -> Int
            -- ^ Size of dictionary.
            -> IO DDict
createDDict dict size = do
  cd <- FFI.checkAlloc "createDDict" $
        FFI.createDDict dict (fromIntegral size)
  fp <- newForeignPtr FFI.p_freeDDict cd
  return (DD fp)

-- | Decompress bytes from source buffer into destination buffer,
-- using a pre-digested dictionary.  The destination buffer must be
-- already allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error description if it fails.
decompressUsingDDict
    :: Ptr DCtx    -- ^ Compression context.
    -> Ptr dst     -- ^ Destination buffer.
    -> Int         -- ^ Capacity of destination buffer.
    -> Ptr src     -- ^ Source buffer.
    -> Int         -- ^ Size of source buffer.
    -> DDict       -- ^ Dictionary.
    -> IO (Either String Int)
decompressUsingDDict ctx dst dstSize src srcSize (DD fp) =
  checkError . withForeignPtr fp $ \ dict ->
  FFI.decompressUsingDDict ctx dst (fromIntegral dstSize)
    src (fromIntegral srcSize) dict

checkError :: IO CSize -> IO (Either String Int)
checkError act = do
  ret <- act
  return $! if FFI.isError ret
            then Left (FFI.getErrorName ret)
            else Right (fromIntegral ret)
