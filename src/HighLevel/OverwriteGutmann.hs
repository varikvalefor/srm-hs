{-# LANGUAGE OverloadedStrings #-}

-- | This module contains @'overwriteGutmann'@, as well as some
-- functions which support @'overwriteGutmann'@.  But these supporting
-- functions are not exported.
module HighLevel.OverwriteGutmann (overwriteGutmann) where
import Base;
import qualified Data.Text.Lazy as T;
import HighLevel.OverwriteRandomNTimes;
import qualified Data.Text.Lazy.IO as T;

-- | Using the GUTMANN method, @overwriteGutmann f@ securely erases @f@.
overwriteGutmann :: FilePath
                 -- ^ This bit is the path of the file which should be
                 -- securely erased.
                 -> IO ();
overwriteGutmann f =
  overwriteRandomNTimes f 4 >>
  mapM_ (writeWPattern f) acceptablePatterns >>
  overwriteRandomNTimes f 4;

-- | @writeWPattern a b@ writes a cycled version of @b@ to @a@.  The
-- length of the output cycled version equals the length of the content
-- of the original @a@.
writeWPattern :: FilePath
              -- ^ This bit is the path of the file to which the pattern
              -- should be written.
              -> T.Text
              -- ^ This value is the pattern which should actually be
              -- written to the file.
              -> IO ();
writeWPattern f p = writeBuffd' f (Just p);

-- | @acceptablePatterns@ is an ordered list of the non-random values
-- which the GUTMANN method mandates.
acceptablePatterns :: [T.Text];
acceptablePatterns = ["\0x55", "\0xAA", "\0x92\0x49\0x24"] ++
                     ["\0x49\0x24\0x92", "\0x24\0x92\0x49", "\0x00"] ++
                     ["\0x11", "\0x22", "\0x33", "\0x44", "\0x55"] ++
                     ["\0x66", "\0x77", "\0x88", "\0x99", "\0xAA"] ++
                     ["\0xBB", "\0xCC", "\0xDD", "\0xEE", "\0xFF"] ++
                     ["\0x92\0x49\0x24", "\0x49\0x24\0x92"] ++
                     ["\0x24\0x92\0x49", "\0x6D\0xB6\0xDB"] ++
                     ["\0xB6\0xDB\0x6D", "\0xDB\0x6D\0xB6"];
