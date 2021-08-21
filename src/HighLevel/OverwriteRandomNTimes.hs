{-# LANGUAGE OverloadedStrings #-}

-- | This module contains @'overwriteRandomNTimes'@.
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
  | n > 0 = writeBuffd' f Nothing >> overwriteRandomNTimes f (n - 1)
  | otherwise = return ();
