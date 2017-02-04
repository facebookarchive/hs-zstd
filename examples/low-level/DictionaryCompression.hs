module Main (main) where

import Codec.Compression.Zstd.Efficient
import Control.Monad (forM_)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath (addExtension, splitExtension)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString as B

main :: IO ()
main = do
  args <- getArgs
  case args of
    "-c":dictName:files -> do
      dict <- (createCDict 3 . mkDict) <$> B.readFile dictName
      compress dict files
    "-d":dictName:files -> do
      dict <- (createDDict . mkDict) <$> B.readFile dictName
      decompress dict files
    "-t":dictSize:dictName:files ->
      case reads dictSize of
        [(size,['k'])] -> train (size * 1024) dictName files
        [(size,['m'])] -> train (size * 1048576) dictName files
        [(size,"")]    -> train size dictName files
        _           -> do
          program <- getProgName
          hPutStrLn stderr $ "usage: " ++ program ++ " -t SIZE DICT [FILES]"
          exitFailure
    _ -> do
      program <- getProgName
      hPutStrLn stderr $ "usage: " ++ program ++ " -[cdt] ARGS"
      exitFailure

compress :: CDict -> [FilePath] -> IO ()
compress dict files = do
  withCCtx $ \ctx ->
    forM_ files $ \file -> do
      content <- compressUsingCDict ctx dict =<< B.readFile file
      B.writeFile (addExtension file "zst") content

decompress :: DDict -> [FilePath] -> IO ()
decompress dict files = do
  withDCtx $ \ctx ->
    forM_ files $ \file -> do
      result <- decompressUsingDDict ctx dict =<< B.readFile file
      let newFile = case splitExtension file of
            (prefix,"zst") -> prefix
            _              -> addExtension file "dzst"
      case result of
        Skip          -> hPutStrLn stderr ("NOTE: skipping " ++ file)
        Error desc    -> hPutStrLn stderr ("ERROR (" ++ file ++ "): " ++ desc)
        Decompress bs -> B.writeFile newFile bs

train :: Int -> FilePath -> [FilePath] -> IO ()
train size dictName files = do
  md <- trainFromSamples size <$> mapM B.readFile files
  case md of
    Left err -> do
      hPutStrLn stderr ("ERROR: " ++ err)
      exitFailure
    Right dict -> B.writeFile dictName (fromDict dict)
