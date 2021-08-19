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
