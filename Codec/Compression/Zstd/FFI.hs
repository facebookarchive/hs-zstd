-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in
-- the LICENSE file in the root directory of this source tree. An
-- additional grant of patent rights can be found in the PATENTS file
-- in the same directory.

{-# LANGUAGE MagicHash #-}

-- |
-- Module      : Codec.Compression.Zstd.FFI
-- Copyright   : (c) 2016-present, Facebook, Inc. All rights reserved.
--
-- License     : BSD3
-- Maintainer  : bryano@fb.com
-- Stability   : experimental
-- Portability : GHC
--
-- Low-level bindings to the native zstd compression library.  These
-- bindings make almost no effort to provide any additional safety or
-- ease of use above that of the C library.  Unless you have highly
-- specialized needs, you should use the streaming or base APIs
-- instead.
--
-- To correctly use the functions in this module, you must read the
-- API documentation in the zstd library's @zstd.h@ include file.  It
-- would also be wise to search elsewhere in this package for uses of
-- the functions you are interested in.

module Codec.Compression.Zstd.FFI
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
    , createCCtx
    , freeCCtx
    , p_freeCCtx
    , compressCCtx

    -- *** Decompression
    , DCtx
    , createDCtx
    , freeDCtx
    , p_freeDCtx
    , decompressDCtx

    -- * Result and error checks
    , isError
    , getErrorName
    , checkError
    , checkAlloc

    -- * Streaming operations
    -- ** Streaming types
    , CStream
    , DStream
    , Buffer(..)
    , In
    , Out

    -- ** Streaming compression
    , cstreamInSize
    , cstreamOutSize
    , createCStream
    , freeCStream
    , p_freeCStream
    , initCStream
    , compressStream
    , endStream

    -- ** Streaming decompression
    , dstreamInSize
    , dstreamOutSize
    , createDStream
    , initDStream
    , decompressStream
    , freeDStream
    , p_freeDStream

    -- * Dictionary-based compression
    , trainFromBuffer
    , getDictID
    , compressUsingDict
    , decompressUsingDict

    -- ** Pre-digested dictionaries
    -- *** Compression
    , CDict
    , createCDict
    , freeCDict
    , p_freeCDict
    , compressUsingCDict

    -- *** Decompression
    , DDict
    , createDDict
    , freeDDict
    , p_freeDDict
    , decompressUsingDDict

    -- * Low-level code
    , c_maxCLevel
    ) where

import Codec.Compression.Zstd.FFI.Types
import Foreign.C.Types (CInt(..), CSize(..), CUInt(..), CULLong(..))
import Foreign.Ptr (FunPtr, nullPtr)
import GHC.CString (unpackCString#)
import GHC.IO.Exception
import GHC.Ptr (Ptr(..))

-- | Compress bytes from source buffer into destination buffer.
-- The destination buffer must be already allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error code if it fails (which can be tested using 'isError').
foreign import ccall unsafe "ZSTD_compress"
    compress :: Ptr dst         -- ^ Destination buffer.
             -> CSize           -- ^ Capacity of destination buffer.
             -> Ptr src         -- ^ Source buffer.
             -> CSize           -- ^ Size of source buffer.
             -> CInt            -- ^ Compression level.
             -> IO CSize

-- | Returns the maximum compression level supported by the library.
foreign import ccall unsafe "ZSTD_maxCLevel"
    c_maxCLevel :: CInt

-- | The maximum compression level supported by the library.
maxCLevel :: Int
maxCLevel = fromIntegral c_maxCLevel

-- | Compute the maximum compressed size of given source buffer.
foreign import ccall unsafe "ZSTD_compressBound"
    compressBound :: Ptr src         -- ^ Source buffer.
                  -> IO CSize

foreign import ccall unsafe "ZSTD_isError"
    c_isError :: CSize -> CUInt

-- | Indicates whether a return value is an error code.
isError :: CSize -> Bool
isError sizeOrError = c_isError sizeOrError /= 0

-- | Gives the description associated with an error code.
--
-- Always returns a valid pointer to a constant string.
foreign import ccall unsafe "ZSTD_getErrorName"
    c_getErrorName :: CSize -> Ptr a

-- | Gives the description associated with an error code.
getErrorName :: CSize -> String
getErrorName cs = unpackCString# (case c_getErrorName cs of Ptr a -> a)

-- | Decompress a buffer.  The destination buffer must be already
-- allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error code if it fails (which can be tested using 'isError').
foreign import ccall unsafe "ZSTD_decompress"
    decompress :: Ptr dst         -- ^ Destination buffer.
               -> CSize           -- ^ Capacity of destination buffer.
               -> Ptr src         -- ^ Source buffer.
               -> CSize
               -- ^ Size of compressed input.  This must be exact, so
               -- for example supplying the size of a buffer that is
               -- larger than the compressed input will cause a failure.
               -> IO CSize

-- | Returns the decompressed size of a compressed payload if known, 0
-- otherwise.
--
-- To discover precisely why a result is 0, follow up with
-- 'getFrameParams'.
foreign import ccall unsafe "ZSTD_getDecompressedSize"
    getDecompressedSize :: Ptr src
                        -> CSize
                        -> IO CULLong

-- | Allocate a compression context.
foreign import ccall unsafe "ZSTD_createCCtx"
    createCCtx :: IO (Ptr CCtx)

-- | Free a compression context.
foreign import ccall unsafe "ZSTD_freeCCtx"
    freeCCtx :: Ptr CCtx -> IO ()

-- | Free a compression context.  For use by a finalizer.
foreign import ccall unsafe "zstd.h &ZSTD_freeCCtx"
    p_freeCCtx :: FunPtr (Ptr CCtx -> IO ())

-- | Compress bytes from source buffer into destination buffer.
-- The destination buffer must be already allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error code if it fails (which can be tested using 'isError').
foreign import ccall unsafe "ZSTD_compressCCtx"
    compressCCtx :: Ptr CCtx    -- ^ Compression context.
                 -> Ptr dst     -- ^ Destination buffer.
                 -> CSize       -- ^ Capacity of destination buffer.
                 -> Ptr src     -- ^ Source buffer.
                 -> CSize       -- ^ Size of source buffer.
                 -> CInt        -- ^ Compression level.
                 -> IO CSize

-- | Compress bytes from source buffer into destination buffer, using
-- a prebuilt dictionary.  The destination buffer must be already
-- allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error code if it fails (which can be tested using 'isError').
foreign import ccall unsafe "ZSTD_compress_usingDict"
    compressUsingDict
        :: Ptr CCtx    -- ^ Compression context.
        -> Ptr dst     -- ^ Destination buffer.
        -> CSize       -- ^ Capacity of destination buffer.
        -> Ptr src     -- ^ Source buffer.
        -> CSize       -- ^ Size of source buffer.
        -> Ptr dict     -- ^ Dictionary.
        -> CSize       -- ^ Size of dictionary.
        -> CInt        -- ^ Compression level.
        -> IO CSize

-- | Compress bytes from source buffer into destination buffer, using
-- a pre-built, pre-digested dictionary.  The destination buffer must
-- be already allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error code if it fails (which can be tested using 'isError').
foreign import ccall unsafe "ZSTD_compress_usingCDict"
    compressUsingCDict
        :: Ptr CCtx    -- ^ Compression context.
        -> Ptr dst     -- ^ Destination buffer.
        -> CSize       -- ^ Capacity of destination buffer.
        -> Ptr src     -- ^ Source buffer.
        -> CSize       -- ^ Size of source buffer.
        -> Ptr CDict   -- ^ Dictionary.
        -> IO CSize

-- | Decompress a buffer, using a prebuilt dictionary.  The
-- destination buffer must be already allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error code if it fails (which can be tested using 'isError').
foreign import ccall unsafe "ZSTD_decompress_usingDict"
    decompressUsingDict
        :: Ptr DCtx        -- ^ Decompression context.
        -> Ptr dst         -- ^ Destination buffer.
        -> CSize           -- ^ Capacity of destination buffer.
        -> Ptr src         -- ^ Source buffer.
        -> CSize
        -- ^ Size of compressed input.  This must be exact, so
        -- for example supplying the size of a buffer that is
        -- larger than the compressed input will cause a failure.
        -> Ptr dict        -- ^ Dictionary.
        -> CSize           -- ^ Size of dictionary.
        -> IO CSize

-- | Decompress a buffer, using a pre-built, pre-digested dictionary.
-- The destination buffer must be already allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error code if it fails (which can be tested using 'isError').
foreign import ccall unsafe "ZSTD_decompress_usingDDict"
    decompressUsingDDict
        :: Ptr DCtx        -- ^ Decompression context.
        -> Ptr dst         -- ^ Destination buffer.
        -> CSize           -- ^ Capacity of destination buffer.
        -> Ptr src         -- ^ Source buffer.
        -> CSize
        -- ^ Size of compressed input.  This must be exact, so
        -- for example supplying the size of a buffer that is
        -- larger than the compressed input will cause a failure.
        -> Ptr DDict       -- ^ Dictionary.
        -> IO CSize

-- | Allocate a decompression context.
foreign import ccall unsafe "ZSTD_createDCtx"
    createDCtx :: IO (Ptr DCtx)

-- | Free a decompression context.
foreign import ccall unsafe "ZSTD_freeDCtx"
    freeDCtx :: Ptr DCtx -> IO ()

-- | Free a decompression context.  For use by a finalizer.
foreign import ccall unsafe "zstd.h &ZSTD_freeDCtx"
    p_freeDCtx :: FunPtr (Ptr DCtx -> IO ())

-- | Decompress a buffer.  The destination buffer must be already
-- allocated.
--
-- Returns the number of bytes written into destination buffer, or an
-- error code if it fails (which can be tested using 'isError').
foreign import ccall unsafe "ZSTD_decompressDCtx"
    decompressDCtx :: Ptr DCtx  -- ^ Decompression context.
                   -> Ptr dst   -- ^ Destination buffer.
                   -> CSize     -- ^ Capacity of destination buffer.
                   -> Ptr src   -- ^ Source buffer.
                   -> CSize
                   -- ^ Size of compressed input.  This must be exact, so
                   -- for example supplying the size of a buffer that is
                   -- larger than the compressed input will cause a failure.
                   -> IO CSize

-- | Recommended size for input buffer.
foreign import ccall unsafe "ZSTD_CStreamInSize"
    cstreamInSize :: CSize

-- | Recommended size for output buffer.
foreign import ccall unsafe "ZSTD_CStreamOutSize"
    cstreamOutSize :: CSize

-- | A context for streaming compression.
data CStream

-- | Create a streaming compression context.  This must be freed using
-- 'freeCStream', or if using a finalizer, with 'p_freeCStream'.
foreign import ccall unsafe "ZSTD_createCStream"
    createCStream :: IO (Ptr CStream)

-- | Free a 'CStream' value.
foreign import ccall unsafe "ZSTD_freeCStream"
    freeCStream :: Ptr CStream -> IO ()

-- | Free a 'CStream' value.  For use by a finalizer.
foreign import ccall unsafe "zstd.h &ZSTD_freeCStream"
    p_freeCStream :: FunPtr (Ptr CStream -> IO ())

-- | Begin a new compression operation.
foreign import ccall unsafe "ZSTD_initCStream"
    initCStream :: Ptr CStream
                -> CInt         -- ^ Compression level.
                -> IO CSize

-- | Consume part or all of an input.
foreign import ccall unsafe "ZSTD_compressStream"
    compressStream :: Ptr CStream -> Ptr (Buffer Out) -> Ptr (Buffer In)
                   -> IO CSize

-- | End a compression stream. This performs a flush and writes a
-- frame epilogue.
foreign import ccall unsafe "ZSTD_endStream"
    endStream :: Ptr CStream -> Ptr (Buffer Out) -> IO CSize

-- | Recommended size for input buffer.
foreign import ccall unsafe "ZSTD_DStreamInSize"
    dstreamInSize :: CSize

-- | Recommended size for output buffer.
foreign import ccall unsafe "ZSTD_DStreamOutSize"
    dstreamOutSize :: CSize

-- | A context for streaming decompression.
data DStream

-- | Create a streaming decompression context.  This must be freed using
-- 'freeDStream', or if using a finalizer, with 'p_freeDStream'.
foreign import ccall unsafe "ZSTD_createDStream"
    createDStream :: IO (Ptr DStream)

-- | Begin a new streaming decompression operation.
foreign import ccall unsafe "ZSTD_initDStream"
    initDStream :: Ptr DStream -> IO CSize

-- | Consume part or all of an input.
foreign import ccall unsafe "ZSTD_decompressStream"
    decompressStream :: Ptr DStream -> Ptr (Buffer Out) -> Ptr (Buffer In)
                     -> IO CSize

-- | Free a 'CStream' value.
foreign import ccall unsafe "ZSTD_freeDStream"
    freeDStream :: Ptr DStream -> IO ()

-- | Free a 'CStream' value.  For use by a finalizer.
foreign import ccall unsafe "zstd.h &ZSTD_freeDStream"
    p_freeDStream :: FunPtr (Ptr DStream -> IO ())

-- | Train a dictionary from a collection of samples.
-- Returns the number size of the resulting dictionary.
foreign import ccall unsafe "ZDICT_trainFromBuffer"
    trainFromBuffer :: Ptr dict
                    -- ^ Preallocated dictionary buffer.
                    -> CSize
                    -- ^ Capacity of dictionary buffer.
                    -> Ptr samples
                    -- ^ Concatenated samples.
                    -> Ptr CSize
                    -- ^ Array of sizes of samples.
                    -> CUInt
                    -- ^ Number of samples.
                    -> IO CSize

-- | Return the identifier for the given dictionary, or zero if
-- not a valid dictionary.
foreign import ccall unsafe "ZDICT_getDictID"
    getDictID :: Ptr dict
              -- ^ Dictionary.
              -> CSize
              -- ^ Size of dictionary.
              -> IO CUInt

-- | Allocate a pre-digested dictionary.
foreign import ccall unsafe "ZSTD_createCDict"
    createCDict :: Ptr dict
                -- ^ Dictionary.
                -> CSize
                -- ^ Size of dictionary.
                -> CInt
                -- ^ Compression level.
                -> IO (Ptr CDict)

-- | Free a pre-digested dictionary.
foreign import ccall unsafe "ZSTD_freeCDict"
    freeCDict :: Ptr CDict -> IO ()

-- | Free a pre-digested dictionary.
foreign import ccall unsafe "zstd.h &ZSTD_freeCDict"
    p_freeCDict :: FunPtr (Ptr CDict -> IO ())

-- | Allocate a pre-digested dictionary.
foreign import ccall unsafe "ZSTD_createDDict"
    createDDict :: Ptr dict
                -- ^ Dictionary.
                -> CSize
                -- ^ Size of dictionary.
                -> IO (Ptr DDict)

-- | Free a pre-digested dictionary.
foreign import ccall unsafe "ZSTD_freeDDict"
    freeDDict :: Ptr DDict -> IO ()

-- | Free a pre-digested dictionary.
foreign import ccall unsafe "zstd.h &ZSTD_freeDDict"
    p_freeDDict :: FunPtr (Ptr DDict -> IO ())

-- | Check that an allocating operation is successful.  If it fails,
-- throw an 'IOError'.
checkAlloc :: String -> IO (Ptr a) -> IO (Ptr a)
checkAlloc name act = do
  addr <- act
  if addr == nullPtr
    then ioError (IOError Nothing ResourceExhausted name
                  "out of memory" Nothing Nothing)
    else return addr

-- | Check whether a 'CSize' has an error encoded in it (yuck!), and
-- report success or failure more safely.
checkError :: IO CSize -> IO (Either String CSize)
checkError act = do
  ret <- act
  return $! if isError ret
            then Left (getErrorName ret)
            else Right ret
