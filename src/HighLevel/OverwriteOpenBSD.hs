{-# LANGUAGE OverloadedStrings #-}

-- |  This module contains @'overwritePseudoOpenBSD'@.
module HighLevel.OverwriteOpenBSD (overwritePseudoOpenBSD) where
import Base;
import System.IO;
import qualified Data.ByteString.Lazy as BSL;

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
overwritePseudoOpenBSD f = mapM_ (writeBuffd' f . Just) values
  where
  values :: [BSL.ByteString]
  values = ["\255", "\0", "\255"];
