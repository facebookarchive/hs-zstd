-- |
-- Module      : Codec.Compression.Zstd.Base.Types
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
-- needs, you should use the streaming API instead.

module Codec.Compression.Zstd.Base.Types
    (
      CDict(..)
    , DDict(..)
    ) where

import Foreign.ForeignPtr (ForeignPtr)
import qualified Codec.Compression.Zstd.FFI.Types as FFI

-- | A pre-digested compression dictionary.
newtype CDict = CD (ForeignPtr FFI.CDict)

-- | A pre-digested decompression dictionary.
newtype DDict = DD (ForeignPtr FFI.DDict)
