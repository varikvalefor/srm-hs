-- | This module provides functions which are used within essentially
-- all other modules of HS-SRM.
--
-- The only things which should be of _major_ interest to most hackers
-- are @'maxRandomBytes'@ and @'writeBuffd\''@.  But feel free to read
-- the entirety of this source file.
module Base where
import System.IO;
import Data.Maybe;
import qualified Data.ByteString.Lazy as BSL;

-- | @sectorSweep f n 'Nothing'@ appends @n@ pseudorandom characters to
-- the file whose path is @f@.
-- @sectorSweep f n p@ otherwise appends @n@ characters of a
-- @'BSL.cycle'@d @p@ to the file whose path is @f@.
--
-- Overwriting entire files with @sectorSweep@ is strictly
-- _not recommended_; the RAM footprint of @sectorSweep _ a _@ is
-- proportional to @a@.
sectorSweep :: FilePath
            -- ^ This value is the path of the file which should be
            -- appended.
            -> Integer
            -- ^ This value is the total length of the data which should
            -- be appended to the file.
            -> Maybe BSL.ByteString
            -- ^ This value, if present, is the data which should be
            -- cycled and appended to the file.  If this value is
            -- 'Nothing', then pseudorandom data is used.
            -> IO ();
sectorSweep f n p = dataToBeWritten >>= BSL.appendFile f
  where
  dataToBeWritten :: IO BSL.ByteString
  dataToBeWritten
    | isNothing p = BSL.pack .
                    map (toEnum . fromEnum) . BSL.unpack . BSL.take n' .
                    BSL.filter isAscii <$> BSL.readFile "/dev/urandom"
    | otherwise = return $ BSL.take n' $ BSL.cycle $ fromJust p
  --
  isAscii = (`elem` (map (toEnum . fromEnum) [' '..'~']))
  n' :: Integral a => a
  n' = fromIntegral n;

-- | @maxRandomBytes@ is the maximum number of random bytes which can be
-- written to a file at any specific time; random data uses a decent bit
-- of RAM, and capping the amount of random data which is stored in RAM
-- mitigates this problem.
maxRandomBytes :: Integer;
maxRandomBytes = 2^16;

-- | @getSize k@ returns the size of the file whose path is @k@.
getSize :: FilePath
        -- ^ This value is the path of the file whose size should be
        -- returned.
        -> IO Integer;
getSize f = do
  howie <- openFile f ReadMode
  size <- hFileSize howie
  hClose howie
  return size;
  -- "@do"@ notation is used strictly because "@do@" notation fits this
  -- process reasonably well.

-- | @blank k@ modifies the file whose path is @k@ such that this file
-- is blank.  This modification is not secure and can potentially be
-- reversed.
blank :: FilePath
      -- ^ This bit is the path of the file which should be blanked.
      -> IO ();
blank f = writeFile f "";

-- | The end result of @writeBuffd f 0 b p@ is equivalent to the end
-- result of @'sectorSweep' f b p@.  This wrapper is used to ensure that
-- the overwriting of large files does not demand exorbitant amounts of
-- RAM.
--
-- Note that @'writeBuffd\''@ also exists.  VARIK finds that
-- @writeBuffd@ is sufficiently low-level to be a bit unwieldy and
-- prefers using @'writeBuffd\''@ over using @writeBuffd@.
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
           -> Maybe BSL.ByteString
           -- ^ This value is the pattern which is cyclically written
           -- to the file.
           --
           -- If this value is 'Nothing', then pseudorandom data is used.
           -> IO ();
writeBuffd f wrtn size sq
  | wrtn < size = sectorSweep f writeSize sq >>
    writeBuffd f (wrtn + writeSize) size sq
  | otherwise = return ()
  where
  writeSize :: Integer
  writeSize = min maxRandomBytes (size - wrtn);

-- | @writeBuffd'@ is a wrapper for @'writeBuffd'@ which handles the
-- fetching of files' sizes and the deletion of such files.  This
-- function is added for the sake of reducing the amount of boilerplate
-- crap which appears within @srm@.
writeBuffd' :: FilePath
            -- ^ This value is the path of the file which should be
            -- overwritten.
            -> Maybe BSL.ByteString
            -- ^ This bit describes the data which is to be written to
            -- the file.  See the documentation of @'writeBuffd'@.
            -> IO ()
writeBuffd' f p = getSize f >>= \s -> blank f >> writeBuffd f 0 s p;
