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
  -- pseudorandom data. 
  optRandom :: Bool
};

instance Options Opt where
  defineOptions = pure Opt
    <*> simpleOption "P" False "Use OpenBSD's old overwriting method."
    <*> simpleOption "G" False "Use the GUTMANN method."
    <*> simpleOption "X" True "Overwrite using k random sweeps.";

main :: IO ();
main = runCommand mane;

mane :: Opt -> [String] -> IO ();
mane opts args
  | optGutmann opts = run overwriteGutmann
  | optBSD opts = run overwritePseudoOpenBSD
  | optRandom opts= mapM_ (flip overwriteRandomNTimes n) (tail args)
  where
  n :: Integer
  n = read $ head args
  --
  run :: (FilePath -> IO ()) -> IO ()
  run k = mapM_ k args;
