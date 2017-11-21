-- TCT Explore
-- Jordan Thayer  2015-04-22T11:01:42-04:00

import Tct
import qualified Tct.Processors
import qualified Tct.Method.Custom as Custom
import System.FilePath.Posix
import qualified Termlib.Problem as Prob
import qualified Tct.Instances as Instances
import qualified Tct.Processor.Args.Instances
import Tct.Processor

exploreProcessors :: Monad m => Prob.Problem -> t -> m (InstanceOf SomeProcessor)
exploreProcessors prob anyproc =
    return $
    case Prob.startTerms prob of
      Prob.TermAlgebra {} -> someInstance $ Instances.dc2012 (Just 60)
      _                   -> someInstance $ Instances.rc2012 (Just 60)

exploreConfig = defaultConfig { makeProcessor   = exploreProcessors }

{-|
  Take a Config and set its problemFile field, leaving everything else
  unchanged.
-}
setProblem :: FilePath -> Config -> Config
setProblem fp config = config { problemFile = fp }

setStrategies :: Config -> [Custom.Strategy] -> Config
setStrategies cfg strats = cfg { strategies = strats}

{- |
   computes the 'power set' of a list. So, we get all 2^n lists with all
   combination of elements removed.
 -}
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)

powerStrategies :: Config -> [[Custom.Strategy]]
powerStrategies cfg = powerSet (strategies cfg)


-- | Show me the config file
displayConfig :: Config -> String
displayConfig = problemFile
instance Show Config where show = displayConfig

runInstances :: FilePath -> Config -> IO ()
runInstances fp baseConfig =
    let pss = powerStrategies baseConfig in
    let allConfigs = map (setStrategies baseConfig) pss in
    mapM_ (runInstance fp) allConfigs

runInstance :: FilePath -> Config -> IO (Either TCTError [TCTWarning])
runInstance fp baseConfig = runErroneous $ runTct (setProblem fp baseConfig)

-- example usage
-- main
-- /home/jtt3620/cage/data/input/TRS/arithmetic/addition.trs

main = do
  instanceName <- getLine
  runInstance instanceName exploreConfig
