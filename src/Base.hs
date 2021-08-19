-- | This module provides functions which are used within essentially
-- all other modules of HS-SRM.
module Base where
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
sectorSweep f n Nothing = generateSuitableRandoms >>= T.appendFile f
  where
  generateSuitableRandoms :: IO T.Text
  generateSuitableRandoms = T.pack . take n . randomRs <$> newStdGen;
sectorSweep f n p = T.appendFile f $ T.take n $ T.cycle p;
