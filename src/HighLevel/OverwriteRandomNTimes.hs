{-# LANGUAGE OverloadedStrings #-}

-- | This module contains @'overwriteRandomNTimes'@.
module HighLevel.OverwriteRandomNTimes (overwriteRandomNTimes) where
import Base;
import System.IO;

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
  | n == 0 = return ()
  | otherwise = error "You shouldn't be back here.";
