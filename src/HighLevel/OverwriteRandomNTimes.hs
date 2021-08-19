{-# LANGUAGE OverloadedStrings #-}

module HighLevel.OverwriteRandomNTimes (overwriteRandomNTimes) where
import Base;
import System.IO;
import qualified Data.Text.Lazy as T;
import qualified Data.Text.Lazy.IO as T;

-- | @overwriteRandomNTimes f n@ overwrites the file whose path is @f@
-- with pseudorandom data @n@ times.
overwriteRandomNTimes :: FilePath
                      -> Integer
                      -> IO ();
overwriteRandomNTimes f n = T.writeFile f "" >> helpy n
  where
  helpy :: Integer -> IO ()
  helpy k
    | k <= 0 = return ()
    | otherwise = openFile f WriteMode >>= hFileSize >>= writeWBuffer f 0;

-- | To avoid using terribly huge amounts of RAM, @writeWBuffer@ is used
-- to generate and write reasonably large amounts of random data to
-- files.
--
-- @writeWBuffer f 0 size@ writes @size@ bytes of random data to the
-- file whose path is @f@.
writeWBuffer :: FilePath
             -> Integer
             -> Integer
             -> IO ();
writeWBuffer f wrtn size
  | wrtn < size = appendectomy >> writeWBuffer f (wrtn + amtToWrite) size
  | otherwise = return ()
  where
  appendectomy :: IO ()
  appendectomy = sectorSweep f amtToWrite Nothing
  --
  amtToWrite :: Integer
  amtToWrite = (size - wrtn) `mod` 2^16;
