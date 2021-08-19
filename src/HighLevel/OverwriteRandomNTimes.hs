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
                      -- ^ This value is the path of the file which is
                      -- to be overwritten.
                      -> Integer
                      -- ^ This value is the number of overwriting
                      -- passes to which the file is subjected.
                      -> IO ();
overwriteRandomNTimes f n
  | n > 0 = getSize f >>= \size -> delete f >> writeBuffd f 0 size
  | otherwise = return ();

-- | To avoid using terribly huge amounts of RAM, @writeBuffd@ is used
-- to generate and write reasonably large amounts of random data to
-- files.
--
-- @writeBuffd f 0 size@ writes @size@ bytes of random data to the
-- file whose path is @f@.
writeBuffd :: FilePath
           -- ^ This value is the path of the file to which the
           -- pseudorandom data is written.
           -> Integer
           -- ^ This value is the amount of data which is already
           -- written to the file.  The initial value should be 0.
           -> Integer
           -- ^ This value is the total amount of data which should be
           -- written to the file.  This value functions as the stopping
           -- point of the recursion.
           -> IO ();
writeBuffd f wrtn size
  | wrtn < size = appendectomy >> writeBuffd f (wrtn + amtToWrite) size
  | otherwise = return ()
  where
  appendectomy :: IO ()
  appendectomy = sectorSweep f amtToWrite Nothing
  --
  amtToWrite :: Integer
  amtToWrite = min maxRandomBytes (size - wrtn);
