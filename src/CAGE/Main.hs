module Main
where
import ProcessTree
import CommonHaskellProcesses
import Control.Monad
import System.Environment
import System.Console.GetOpt
import System.Directory

data Options = Options { jarDir :: String
                       , outDir :: Maybe String } deriving Show

-- | Translate Jar Dir to default output dir, in the case
outDirFromJarDir :: String -> String
outDirFromJarDir jd =
    jd ++ "/CAGE-Out/"

setOutDir :: Options -> Options
setOutDir o @ (Options jd Nothing) =
    o { outDir = Just $ outDirFromJarDir jd }
setOutDir o = o

-- Default args for CAGE
defaultOpts :: Options
defaultOpts = Options "." Nothing

-- list of input arguments
optionList :: [(Char, String, ArgDescr (Options -> Options), String)]
optionList = [('i', "input", ReqArg (\s o -> o {jarDir = s }) "DIR",
                  "Input Directory (contains jar files)"),
              ('o', "output", ReqArg (\s o -> o {outDir = Just s }) "DIR",
                  "Output Directory (*.inttrs, *.ints, *.cmpl, etc)")
             ]

options :: [OptDescr (Options -> Options)]
options = map (\(a, b, c, d) -> Option [a] [b] c d) optionList

optionPrint :: String
optionPrint = concat $ map (\(x,_,_,_) -> [' ','-',x]) optionList

header::String
header = "Usage: cage [-i DIR | -o DIR ]"

usage :: [String] -> IO()
usage errs = putStrLn $ concat errs ++ "\n" ++ usageInfo header options

validateArgs :: Options -> IO [String]
validateArgs o @ (Options _ Nothing) = validateArgs $ setOutDir o
validateArgs (Options inDir (Just od)) = do
  inOK <- doesDirectoryExist inDir
  outNeedsMade <- doesDirectoryExist od
  if inOK
    then if outNeedsMade
         then createDirectoryIfMissing outNeedsMade `seq` return []
         else return []
    else return [inDir ++ "does Not Exist"]

sampleEndToEnd :: [Char] -> [Char] -> Process
sampleEndToEnd jd od =
    koatGroup { subProcesses = [aprove] }
    where aprove = makeAProVEFromJars jd od
          koatGroup = makeKoatsFromINTTRS od

run :: Options -> IO ()
run o @ (Options _ Nothing) = run $ setOutDir o
run o @ (Options jd (Just artifactDir)) = do
  go <- validateArgs o
  case go of
    [] ->
        let result = runTree (sampleEndToEnd jd artifactDir) in
        let output = liftM prettyPrint result in
        putStrLn =<< liftM ("\n\nFinal Tree:\n" ++) output
    _ -> usage go

main :: IO ()
main = do
  argList <- getArgs
  case getOpt Permute options argList of
    (optArgs, _, []) ->
        let opts = foldl (\o f -> f o) defaultOpts optArgs in
        run opts
    (_,_,errs) -> usage errs
  where

