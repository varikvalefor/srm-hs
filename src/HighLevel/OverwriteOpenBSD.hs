{-# LANGUAGE OverloadedStrings #-}

-- |  This module contains @'overwritePseudoOpenBSD'@ and some functions
-- which support @'overwritePseudoOpenBSD'@.
module HighLevel.OverwriteOpenBSD (overwritePseudoOpenBSD) where
import Base;
import System.IO;
import qualified Data.Text.Lazy as T;
import qualified Data.Text.Lazy.IO as T;

-- | @overwritePseudoOpenBSD k@ overwrites all bytes of @k@ with
-- @\'\\FF\'@, then @\'\\00\'@, and then @\'\\FF\'@.  According to the
-- manual page of the original @srm@, this functionality is equivalent
-- to OpenBSD's @rm -P k@.  However, OpenBSD's @rm -P@ functionality has
-- changed, and this functionality no longer resembles the functionality
-- of @rm -P@; as such, the term "pseudo" is used.
overwritePseudoOpenBSD :: FilePath
                       -- ^ This file path refers to the file which
                       -- should be overwritten.
                       -> IO ();
overwritePseudoOpenBSD f = getSize f >>= writeBuffdAll f
  where
  writeBuffdAll :: FilePath -> Integer -> IO ()
  writeBuffdAll f s = mapM_ overwriteWith ["\255", "\0", "\255"]
    where
    overwriteWith :: T.Text -> IO ()
    overwriteWith a = delete f >> writeBuffd f s 0 a;

-- | The end result of @writeBuffd f a b p@ is equivalent to the end
-- result of @'sectorSweep' f b p@.  This wrapper is used to ensure that
-- the overwriting of large files does not demand exorbitant amounts of
-- RAM.
writeBuffd :: FilePath
        -- ^ This thing is the path to the file which is overwritten.
        -- PROTIP: Using @"/dev/null"@ as this value is a half-decent
        -- method of wasting processing power.
        -> Integer
        -- ^ This value is the number of bytes which are already
        -- written.  This value is essentially only nonzero within
        -- @writeBuffd@.
        -> Integer
        -- ^ This value is the total number of bytes which should be
        -- written.
        -> T.Text
        -- ^ This value is the pattern which is written.
        -> IO ();
writeBuffd f wrtn size sq
  | wrtn < size = sectorSweep f writeSize (Just sq) >>
    writeBuffd f (wrtn + writeSize) size sq
  | otherwise = return ()
  where
  writeSize :: Integer
  writeSize = min maxRandomBytes (size - wrtn);
