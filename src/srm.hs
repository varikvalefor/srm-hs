import Options;
import Data.Bool;
import Control.Monad;
import System.Directory;
import System.Environment;
import Control.Applicative;
import HighLevel.OverwriteSimple;
import HighLevel.OverwriteGutmann;
import HighLevel.OverwriteOpenBSD;
import HighLevel.OverwriteFemaleDeer;
import HighLevel.OverwriteRandomNTimes;

-- | For all 'Opt' @k@, @k@ describes the arguments of the program.
--
-- Within the remainder of this documentation, @x@ denotes the main
-- 'Opt' variable of @srm@.
data Opt = Opt {
  -- | @optGutmann x@ iff the file should be overwritten per the GUTMANN
  -- method.
  optGutmann :: Bool,
  -- | @optBSD x@ iff the file should be overwritten as the old OpenBSD
  -- @rm -P@ overwrites files.
  optBSD :: Bool,
  -- | @optRandom x == n@ iff the file should be overwritten with
  -- pseudorandom data @n@ times.
  optRandom :: Integer,
  -- | @optSimple x@ iff the file should be overwritten using the
  -- "simple" method of the original @srm@.
  optSimple :: Bool,
  -- | @optRecurse x@ iff the contents of directories should be
  -- deleted.
  optRecurse :: Bool,
  -- | @optFemaleDeer x@ iff the file should be overwritten using the
  -- Department of Energy thing which is implemented in the original
  -- @srm@.
  optFemaleDeer :: Bool
};

instance Options Opt where
  defineOptions = pure Opt
              <*> optGutmann'
              <*> optBsd'
              <*> optRand'
              <*> optSimple'
              <*> optRecurse'
              <*> optFemaleDeer'
    where
    optGutmann' = defineOption optionType_bool (\o -> o
      {
        optionShortFlags = "G",
        optionDefault = False,
        optionDescription = "Use the GUTMANN method."
      })
    optBsd' = defineOption optionType_bool (\o -> o
      {
        optionShortFlags = "P",
        optionDefault = False,
        optionDescription = "Use OpenBSD's old overwriting method."
      })
    optRand' = defineOption optionType_integer (\o -> o
      {
        optionShortFlags = "X",
        optionDefault = 0,
        optionDescription = "Overwrite using k random sweeps, " ++
          "where k is the argument of this option."
      })
    optSimple' = defineOption optionType_bool (\o -> o
      {
        optionShortFlags = "s",
        optionDefault = True,
        optionDescription = "Overwrite with a single pass of null " ++
          "bytes."
      })
    optRecurse' = defineOption optionType_bool (\o -> o
      {
        optionShortFlags = "rR",
        optionDefault = True,
        optionDescription = "Recursively delete the contents of \
                            \directories."
      })
    optFemaleDeer' = defineOption optionType_bool (\o -> o
      {
        optionShortFlags = "E",
        optionDefault = False,
        optionDescription = "Use the original srm(1)'s Department of \
                            \Energy wiping method."
      });

main :: IO ();
main = runCommand overwrite >> runCommand delete;

-- | @mane a k@ overwrites the files which are specified in @k@ via the
-- process which is specified in @a@, where @a@ describes the options of
-- @a@ and @k@ is a ['String']-based list of the paths of the files
-- which should be overwritten.
overwrite :: Opt
          -- ^ A description of the options which are passed to @srm@
          -> [String]
          -- ^ The paths of the files which should be overwritten
          -> IO ();
overwrite opts args
  | optRecurse opts = files args >>= overwrite opts {optRecurse = False}
  | optGutmann opts = run overwriteGutmann
  | optBSD opts = run overwritePseudoOpenBSD
  | optFemaleDeer opts = run overwriteFemaleDeer
  | optRandom opts > 0 = run $ flip overwriteRandomNTimes n
  | optSimple opts = run overwriteSimple
  | otherwise = error "Homeboy, what is your problem?"
  where
  n :: Integer
  n = optRandom opts
  --
  run :: (FilePath -> IO ()) -> IO ()
  run k = mapM_ k args;


-- | @files k@ returns a list which contains /all/ files which are
-- contained by the directories which are specified by @k@.  If @k@
-- contains the paths of some standard files, then the paths of these
-- files are also returned.
--
-- Recursion is used.
files :: [FilePath]
      -- ^ For any element of this list $T$, if $T$ is the path of a
      -- directory, then $T$ is "replaced" by a list of the
      -- subdirectories and folders which are contained by $T$.  If
      -- $T$ is the path of a plain ol' file, then $T$ remains the same.
      -> IO [String];
files k = bool (pure k) recurse =<< anyM dde k
  where
  recurse = files =<< concat <$> mapM contentsOrId k
  dde = doesDirectoryExist
  subDirs = concat <$> mapM contentsOrId k
  anyM f l = or <$> mapM f l
  getGoodDirCont = liftM (filterOut ["..", "."]) . getDirectoryContents
  filterOut b = filter (not . (`elem` b))
  contentsOrId j = bool (pure $ pure j) (contentsOf j) =<< dde j
  contentsOf j = map ((j ++ "/") ++) <$> getGoodDirCont j;

-- | @delete _ k@ deletes the files which are specified in @k@, where
-- @k@ is a ['String']-based list of the file paths which are the
-- arguments of @srm@.
delete :: Opt
       -- ^ This bit describes the options which are passed to @srm@...
       -- and actually goes unused.
       -> [String]
       -- ^ This thing contains the paths of the files which are to be
       -- deleted.
       -> IO ();
delete _ = mapM_ removePathForcibly;
