-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in
-- the LICENSE file in the root directory of this source tree. An
-- additional grant of patent rights can be found in the PATENTS file
-- in the same directory.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module QuickCheckUtils
    (
      CLevel(..)
    , NEBS(..)
    , rechunkList
    , rechunk
    , smallArbitrary
    , stream
    , throwsException
    , unsquare
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

newtype NEBS = NE { fromNE :: ByteString }
  deriving (Eq, Ord, Read, Show)

instance Arbitrary ByteString where
    arbitrary     = pack `fmap` arbitrary
    shrink        = map pack . shrink . unpack

instance Arbitrary NEBS where
    arbitrary     = (NE . pack . getNonEmpty) `fmap` arbitrary
    shrink        = map (NE . pack . getNonEmpty) . shrink .
                    NonEmpty . unpack . fromNE

newtype CLevel = CLevel { fromCLevel :: Int }
  deriving (Eq, Ord, Read, Show)

instance Arbitrary CLevel where
    arbitrary = fmap CLevel (choose (1, maxCLevel))
    shrink    = map CLevel . filter (>0) . shrink . fromCLevel

rechunkList :: [Int] -> ByteString -> [ByteString]
rechunkList cs0 bs0 = go cs0 bs0
  where
    go _      bs
      | B.null bs = []
    go []     bs  = [bs]
    go (c:cs) bs
      | c <= 0    = go cs bs
      | otherwise = let (h,t) = B.splitAt c bs
                    in h : go cs t

rechunk :: [Int] -> ByteString -> L.ByteString
rechunk cs bs = L.fromChunks (rechunkList cs bs)

stream :: IO S.Result -> [Int] -> ByteString -> [ByteString]
stream act cs s = unsafePerformIO $ go (rechunkList cs s) =<< act
  where
    go _      (S.Error w e) = error $ w ++ ": " ++ e
    go bs     (Produce p k) = (p:) <$> (go bs =<< k)
    go (b:bs) (Consume k)   = go bs =<< k b
    go []     (Consume k)   = done =<< k B.empty
    go _      wtf           = error $ "stream go: unexpected " ++ show wtf
    done (Produce p k) = (p:) <$> (done =<< k)
    done (Done p)      = return [p]
    done (S.Error w e) = error $ w ++ ": " ++ e
    done wtf           = error $ "stream done: unexpected " ++ show wtf

throwsException :: (a -> b) -> a -> Bool
throwsException f a =
  let ret = return :: b -> IO b
      try = E.try :: IO b -> IO (Either E.SomeException b)
  in case unsafePerformIO (try (E.evaluate (ret (f a)))) of
       Left _err -> True
       _         -> False

-- For tests that have O(n^2) running times or input sizes, resize
-- their inputs to the square root of the originals.
unsquare :: (Arbitrary a, Show a, Testable b) => (a -> b) -> Property
unsquare = forAll smallArbitrary

smallArbitrary :: (Arbitrary a, Show a) => Gen a
smallArbitrary = sized $ \n -> resize (smallish n) arbitrary
  where smallish = round . (sqrt :: Double -> Double) . fromIntegral . abs
