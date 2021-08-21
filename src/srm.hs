import Options;
import System.Environment;
import Control.Applicative;
import HighLevel.OverwriteGutmann;
import HighLevel.OverwriteOpenBSD;
import HighLevel.OverwriteRandomNTimes;

-- | For all 'Opt' @k@, @k@ describes the arguments of the program.
data Opt = Opt {
  -- | @optGutmann x@ iff the file should be overwritten per the GUTMANN
  -- method.
  optGutmann :: Bool,
  -- | @optBSD x@ iff the file should be overwritten as the old OpenBSD
  -- @rm -P@ overwrites files.
  optBSD :: Bool,
  -- | @optRandom x@ iff the file should be overwritten with
  -- pseudorandom data @x@ times. 
  optRandom :: Integer
};

instance Options Opt where
  defineOptions = pure Opt
    -- optGutmann
    <*> defineOption optionType_bool (\o -> o
        {
          optionShortFlags = "G",
          optionDefault = False,
          optionDescription = "Use the GUTMANN method."
        })
    -- optBSD
    <*> defineOption optionType_bool (\o -> o 
        {
          optionShortFlags = "P",
          optionDefault = False,
          optionDescription = "Use OpenBSD's old overwriting method."
        })
    -- optRandom
    <*> defineOption optionType_integer (\o -> o
        {
          optionShortFlags = "X",
          optionDefault = 0,
          optionDescription = "Overwrite using k random sweeps, " ++
            "where k is the argument of this option."
        });

main :: IO ();
main = runCommand mane;

-- | @mane a k@ determines the thing which should be done, where @a@
-- describes the options which are passed to @srm@ and @k@ is a
-- ['String']-based list of the paths of the files which should be
-- overwritten.
mane :: Opt
     -- ^ A description of the options which are passed to @srm@
     -> [String]
     -- ^ The paths of the files which should be overwritten
     -> IO ();
mane opts args
  | optGutmann opts = run overwriteGutmann
  | optBSD opts = run overwritePseudoOpenBSD
  | optRandom opts > 0 = mapM_ (flip overwriteRandomNTimes n) args
  | otherwise = error "Homeboy, what is your problem?"
  where
  n :: Integer
  n = optRandom opts
  --
  run :: (FilePath -> IO ()) -> IO ()
  run k = mapM_ k args;
