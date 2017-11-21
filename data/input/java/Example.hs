module Example
where 
import Test.Tasty
import Test.Tasty.Program

import Data.List
import System.Directory
import Control.Monad

disallowed = [".", "..", "dist"]
intTRSOut = "/var/tmp"

aproveTimeOut = "60"

-- | extension -> directory -> Files having that extension in directory
findSomething :: String -> String -> IO [String]
findSomething ext dir = do
  fileNames <- getDirectoryContents dir
  return $ map (dir ++) $  filter (isSuffixOf ext) fileNames

findDirectories :: String -> IO [String]
findDirectories root = do
    possible <- getDirectoryContents root
    filtered <- filterM doesDirectoryExist possible
    return $ map (\s -> s ++ "/") $ filter (\s -> not $ elem s disallowed) filtered

makeJavaBuild :: String -> TestTree
makeJavaBuild dirName =
    testProgram ("Build " ++ dirName) "make" ["programs"] (Just dirName)

-- We only really need to worry about this if sandboxes aren't working as intended
makeJavaClean :: String -> TestTree
makeJavaClean dirName =
    testProgram ("Clean " ++ dirName) "make" ["clean"] (Just dirName)

makeAllJavaBuilds :: String -> IO [TestTree]
makeAllJavaBuilds rootDir = do
  allDirs <- findDirectories rootDir
  return $ map makeJavaBuild allDirs


makeAllAproveRuns :: String -> IO [TestTree]
makeAllAproveRuns rootDir = do
  allDirs <- findDirectories rootDir
  strings <- mapM (findSomething ".jar") allDirs
  return $ zipWith groupOfDir allDirs strings
    where programOfJar dir jarPath =
              testProgram ("AProVE " ++ jarPath) "aprove-silent"
                              ["-t", aproveTimeOut, "-o", intTRSOut,
                               jarPath] Nothing
          groupOfDir dir jars =
              testGroup ("AProVE " ++ dir ++ " artifacts") $
                        map (programOfJar dir) jars

makeAllIntTRSTrans :: String -> IO [TestTree]
makeAllIntTRSTrans inttrsDir = do
  allFiles <- findSomething ".inttrs" inttrsDir
  return $ map makeTrans allFiles
      where makeTrans path =
                testProgram ("Translate " ++ path) "inttrsToITS" [path] Nothing

