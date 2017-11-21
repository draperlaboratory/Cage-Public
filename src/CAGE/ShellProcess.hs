-- Modelling a step in the CAGE ShellProcess
-- Jordan Thayer  2015-05-13T13:43:16-04:00

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module ShellProcess
( InputRedirection(..)
, Flag(..)
, CLI(..)
, ShellProcess(..)
, shellString
, invokeSP
) where

import System.Directory
import System.Process
import Control.Monad()
import Control.Exception.Base
import System.IO.Unsafe()

data InputRedirection =
    IOR { input :: Maybe String
        , output :: Maybe String } deriving (Show,Eq)

data Flag = Long String | Short Char | Abnorm String deriving (Eq, Show)

data CLI = BareFlag Flag | ArgFlag (Flag, String) | BareArg String deriving (Eq, Show)

data ShellProcess =
    ShellProcess { baseBin    :: String
                 , redirects  :: InputRedirection
                 , args       :: [CLI]
                 , workingDir :: String } deriving (Show, Eq)

-- Different from Show because I want those to preserve data types and such.
-- Here, we'll strip those away because we must.
-- | Converts various portions of a shell command into appropriate strings.
class Shellable a where shellString :: a -> String

instance Shellable String where
    shellString s = s

instance Shellable Flag where
    shellString (Long s) = "--" ++ s
    shellString (Abnorm s) = "-" ++ s
    shellString (Short c) = '-' : c : []

instance Shellable CLI where
    shellString (BareFlag f) = shellString f
    shellString (ArgFlag (f,a)) = shellString f ++ ' ' : a
    shellString (BareArg a) = a

instance Shellable [CLI] where
    shellString = concatMap ((' ' :) .  shellString)

instance Shellable ShellProcess where
    shellString (ShellProcess bin (IOR stdin stdout) alist _) =
        inStr ++ bin ++ shellString alist ++ outStr
        where inStr = case stdin of
                        Nothing -> ""
                        Just s -> "cat " ++ shellString s ++ " | "
              outStr = case stdout of
                         Nothing -> ""
                         Just s -> ' ' : '>' : ' ' : shellString s

{- |
   Gets a string representing stdin for the given ShellProcess.

   We're not using pipes because we want to save intermediate files,
   so this function exists to read one of the intermediate files so that it
   can be used further down the line as stdin.
 -}
getInput :: ShellProcess -> IO String
getInput sp =
      case input (redirects sp) of
        Nothing -> evaluate ""
        Just fp -> readFile fp

{- |
   If a shell process has both output redirection and output, creates a file
   and fills it with the process' output as if it had been done with
   redirection.

   Returns true if the operation is successful, false otherwise
 -}
writeOutput :: ShellProcess -> String -> IO Bool
writeOutput (ShellProcess _ (IOR _ Nothing) _ _) _ = evaluate True
writeOutput (ShellProcess _ (IOR _ (Just fname)) _ _) stdout = do
  result <- try (writeFile fname stdout) :: IO (Either IOError ())
  case result of
    Right _ -> return True
    Left _ -> return False

outputExists :: ShellProcess -> IO Bool
-- You might think this would be true, but we can't easily tell if
-- a program with no redirection has already run.
outputExists (ShellProcess _ (IOR _ Nothing) _ _) = return False
outputExists (ShellProcess _ (IOR _ (Just path)) _ _) = doesFileExist path

{- |
   Run a shell process.  Return true if the process failed.

   We want that logic here because we intend to filter failing shell commands
   for error reporting.
 -}
runSP :: ShellProcess -> IO Bool
runSP sp = do
  -- I'm just guessing that the string comparison is cheaper than env. changes
  _ <- if wd /= "." then setCurrentDirectory wd else evaluate ()
  print (bin ++ " " ++ (concatMap (++ " " )argstring))
  stdin <- getInput sp
  stdout <- try (readProcess bin argstring stdin) :: IO (Either IOError String)
  val <- case stdout of
           Right s -> writeOutput sp s
           Left _ -> evaluate False
  return $ not val
    where
      wd = workingDir sp
      argstring = map shellString $ args sp
      bin = baseBin sp

invokeSP :: ShellProcess -> IO Bool
invokeSP sp = do
  alreadyRun <- outputExists sp
  if alreadyRun
    then return False
    else runSP sp
