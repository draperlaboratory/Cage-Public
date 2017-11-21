module Main
where
import PolyRing
import Graph
import System.Directory
import Control.Exception.Base
import System.Environment
import System.Console.GetOpt

data OutputClass = PolyRing | Undefined String deriving (Show, Eq)

data Options = Options { humanReadable :: Bool
                       , size :: Int
                       , outputTarget :: OutputClass
                       , outputDir :: Maybe String } deriving Show

parseTarget :: String -> OutputClass
parseTarget s
    | s == "POLYRING" = PolyRing
    | otherwise = Undefined s

typesString = "POLYRING"

parseInt :: String -> Int
parseInt s = (read :: (String -> Int)) s

parseBool :: String -> Bool
parseBool s = (read :: (String -> Bool)) s

-- Default args for JavaGen
defaultOpts :: Options
defaultOpts = Options False 10 PolyRing (Just ".")


-- list of input arguments
optionList :: [(Char, String, ArgDescr (Options -> Options), String)]
optionList = [('h', "human-readable",
               ReqArg (\b o -> o {humanReadable = parseBool b }) "BOOL",
               "Add System.out.printlns to generated Java"),
              ('s', "size",
               ReqArg (\s o -> o {size = parseInt s }) "INT",
               "How big is the class to be generated"),
              ('o', "output-dir",
               ReqArg (\s o -> o {outputDir = Just s}) "DIR",
               "Output Directory (defaults \".\")"),
              ('t', "type",
               ReqArg (\s o -> o {outputTarget = parseTarget s}) "TYPE",
               "Type of Java class to generate: " ++ typesString)
             ]

options :: [OptDescr (Options -> Options)]
options = map (\(a, b, c, d) -> Option [a] [b] c d) optionList

header::String
header = "Usage: JavaGen [ -h BOOL | -s INT | -t TYPE | -o DIR ]"

usage :: [String] -> IO()
usage errs = putStrLn $ concat errs ++ "\n" ++ usageInfo header options

writeJavaFile :: String -> String -> IO Bool
writeJavaFile path fileContents = do
    result <- try (writeFile path fileContents) :: IO (Either IOError ())
    case result of
      Right _ -> return True
      Left _ -> return False


run :: Options -> IO Bool
run opts
    | ot == PolyRing = writeJavaFile fname contents
    | otherwise = usage ["Unrecognized Type, try using one of " ++ typesString]
                  `seq` return False
    where hr = humanReadable opts
          ot = outputTarget opts
          sz = size opts
          contents = wholeClass hr sz
          fname = javaPath hr sz

main :: IO Bool
main = do
  argList <- getArgs
  case getOpt Permute options argList of
    (optArgs, _, []) ->
        let opts = foldl (\o f -> f o) defaultOpts optArgs in
        run opts
    (_,_,errs) -> usage errs `seq` return False
