{-# LANGUAGE OverloadedStrings #-}

-- |  This module contains @'overwritePseudoOpenBSD'@.
module HighLevel.OverwriteOpenBSD (overwritePseudoOpenBSD) where
import Base;
import System.IO;
import qualified Data.Text.Lazy as T;
import qualified Data.Text.Lazy.IO as T;

-- | @overwritePseudoOpenBSD k@ overwrites all bytes of @k@ with
-- @\'\\0xFF\'@, then @\'\\0x00\'@, and then @\'\\0xFF\'@.
--
-- According to the manual page of the original @srm@, this
-- functionality is equivalent to OpenBSD's @rm -P k@.  However,
-- OpenBSD's @rm -P@ functionality has changed, and this functionality
-- no longer resembles the functionality of @rm -P@; as such, the term
-- "pseudo" is used.
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
    overwriteWith a = delete f >> writeBuffd f s 0 (Just a);
