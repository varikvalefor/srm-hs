{-# LANGUAGE OverloadedStrings #-}

-- | This module contains @'overwriteGutmann'@, as well as some
-- functions which support @'overwriteGutmann'@.  But these supporting
-- functions are not exported.
module HighLevel.OverwriteGutmann (overwriteGutmann) where
import Base;
import HighLevel.OverwriteRandomNTimes;
import qualified Data.ByteString.Lazy as BSL;

-- | Using the GUTMANN method, @overwriteGutmann f@ securely erases @f@.
overwriteGutmann :: FilePath
                 -- ^ This bit is the path of the file which should be
                 -- securely erased.
                 -> IO ();
overwriteGutmann f =
  overwriteRandomNTimes f 4 >>
  mapM_ (writeBuffd' f . Just) acceptablePatterns >>
  overwriteRandomNTimes f 4;

-- | @acceptablePatterns@ is an ordered list of the non-random values
-- which the GUTMANN method mandates.
acceptablePatterns :: [BSL.ByteString];
acceptablePatterns = ["\0x55", "\0xAA", "\0x92\0x49\0x24",
                      "\0x49\0x24\0x92", "\0x24\0x92\0x49", "\0x00",
                      "\0x11", "\0x22", "\0x33", "\0x44", "\0x55",
                      "\0x66", "\0x77", "\0x88", "\0x99", "\0xAA",
                      "\0xBB", "\0xCC", "\0xDD", "\0xEE", "\0xFF",
                      "\0x92\0x49\0x24", "\0x49\0x24\0x92",
                      "\0x24\0x92\0x49", "\0x6D\0xB6\0xDB",
                      "\0xB6\0xDB\0x6D", "\0xDB\0x6D\0xB6"];
