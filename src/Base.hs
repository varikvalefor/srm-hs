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
