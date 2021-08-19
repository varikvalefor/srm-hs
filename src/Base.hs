-- | This module provides functions which are used within essentially
-- all other modules of HS-SRM.
module Base where
import GHC.Int;
import System.IO;
import Data.Maybe;
import System.Random;
import qualified Data.Text.Lazy as T;
import qualified Data.Text.Lazy.IO as T;

-- | @sectorSweep f n 'Nothing'@ appends @n@ pseudorandom characters to
-- the file whose path is @f@.
-- @sectorSweep f n p@ otherwise appends @n@ characters of a
-- @'T.cycle'@d @p@ to the file whose path is @f@.
sectorSweep :: FilePath
            -- ^ This value is the path of the file which should be
            -- appended.
            -> Integer
            -- ^ This value is the total length of the data which should
            -- be appended to the file.
            -> Maybe T.Text
            -- ^ This value, if present, is the data which should be
            -- cycled and appended to the file.  If this value is
            -- 'Nothing', then pseudorandom data is used.
            -> IO ();
sectorSweep f n p = dataToBeWritten >>= T.appendFile f
  where
  dataToBeWritten :: IO T.Text
  dataToBeWritten
    | isNothing p = T.pack . take n' . randomRs (' ', '~') <$> newStdGen
    | otherwise = return $ T.take n' $ T.cycle $ fromJust p
  --
  n' :: Integral a => a
  n' = fromIntegral n;

-- | @maxRandomBytes@ is the maximum number of random bytes which can be
-- written to a file at any specific time; random data uses a decent bit
-- of RAM, and capping the amount which is stored in RAM mitigates this
-- problem.
maxRandomBytes :: Integer;
maxRandomBytes = 2^16;

-- | @getSize k@ returns the size of the file whose path is @k@.
getSize :: FilePath
        -- ^ The path of the file whose size should be returned
        -> IO Integer;
getSize f = do
  howie <- openFile f ReadMode
  size <- hFileSize howie
  hClose howie
  return size;
  -- "@do"@ notation is used strictly because "@do@" notation fits this
  -- process reasonably well.

-- | @delete k@ modifies the file whose path is @k@ such that this file
-- is blank.  This modification is not secure and can potentially be
-- reversed.
delete :: FilePath
       -> IO ();
delete f = writeFile f "";

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
