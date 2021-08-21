{-# LANGUAGE OverloadedStrings #-}

-- | This module contains @'overwriteSimple'@.
module HighLevel.OverwriteSimple (overwriteSimple) where
import Base;

-- | @overwriteSimple f@ overwrites @f@ with null bytes.
overwriteSimple :: FilePath
                -- ^ This bit is the path of the file which should be
                -- securely erased.
                -> IO ();
overwriteSimple f = getSize f >>= \s -> writeBuffd f 0 s (Just "\00");
