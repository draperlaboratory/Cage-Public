-- Modelling a step in the CAGE ShellProcess
-- Jordan Thayer  2015-05-13T13:43:16-04:00

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module ShellProcess
( InputRedirection(..)
, Flag(..)
, ShellProcess(..)
, Setting
, CommandLineArgument
, shellString
, invokeSP
) where

import System.Process
import Control.Monad()
import Control.Exception.Base
import System.IO.Unsafe()

data InputRedirection =
    IOR { input :: Maybe String
        , output :: Maybe String } deriving (Show,Eq)

data Flag = Long String | Short Char | Abnorm String | Bare String
            deriving (Eq, Show)

type Setting = String
type CommandLineArgument = (Flag , Setting)

data ShellProcess =
    ShellProcess { baseBin   :: String
                 , redirects :: InputRedirection
                 , args      :: [CommandLineArgument] } deriving (Show, Eq)

class Shellable a where shellString :: a -> String

instance Shellable String where
    shellString s = s

instance Shellable Flag where
    shellString (Long s) = "--" ++ s
    shellString (Abnorm s) = "-" ++ s
    shellString (Short c) = ['-' , c]
    shellString (Bare s) = s

instance Shellable CommandLineArgument where
    shellString (flag, setting) =
        if setting == ""
        then shellString flag
        else shellString flag ++ ' ' : setting

instance Shellable [CommandLineArgument] where
    shellString = concatMap ((' ' :) .  shellString)

instance Shellable ShellProcess where
    shellString (ShellProcess bin ior alist) =
        inStr ++ bin ++ shellString alist ++ outStr
        where inStr = case input ior of
                        Nothing -> ""
                        Just s -> "cat " ++ shellString s ++ " | "
              outStr = case output ior of
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
writeOutput (ShellProcess _ (IOR _ Nothing) _) _ = return True
writeOutput (ShellProcess _ (IOR _ (Just fname)) _) stdout = do
  result <- try (writeFile fname stdout) :: IO (Either IOError ())
  case result of
    Right _ -> return True
    Left _ -> return False

{- |
   Run a shell process.  Return true if the process failed.

   We want that logic here because we intend to filter failing shell commands
   for error reporting.
 -}
invokeSP :: ShellProcess -> IO Bool
invokeSP sp = do
  stdin <- getInput sp
  stdout <- try (readProcess bin argstring stdin) :: IO (Either IOError String)
  val <- case stdout of
           Right s -> writeOutput sp s
           Left _ -> return False
  return $ not val
    where
      argstring = map shellString $ args sp
      bin = baseBin sp

