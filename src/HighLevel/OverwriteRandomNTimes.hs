{-# LANGUAGE OverloadedStrings #-}

-- | This module contains @'overwriteRandomNTimes'@ and some functions
-- which support @'overwriteRandomNTimes'@.
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
    | k > 0 = getSize f >>= writeBuffd f 0
    | otherwise = return ();

-- | @getSize k@ returns the size of the file whose path is @k@.
getSize :: FilePath
        -- ^ The path of the file whose size should be returned
        -> IO Integer;
getSize f = do
  howie <- openFile f WriteMode
  size <- hFileSize howie
  hClose howie
  return size;

-- | To avoid using terribly huge amounts of RAM, @writeBuffd@ is used
-- to generate and write reasonably large amounts of random data to
-- files.
--
-- @writeBuffd f 0 size@ writes @size@ bytes of random data to the
-- file whose path is @f@.
writeBuffd :: FilePath
             -> Integer
             -> Integer
             -> IO ();
writeBuffd f wrtn size
  | wrtn < size = appendectomy >> writeBuffd f (wrtn + amtToWrite) size
  | otherwise = return ()
  where
  appendectomy :: IO ()
  appendectomy = sectorSweep f amtToWrite Nothing
  --
  amtToWrite :: Integer
  amtToWrite = (size - wrtn) `mod` 2^16;
