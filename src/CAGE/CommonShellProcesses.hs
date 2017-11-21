-- Common Shell Processes for CAGE system
-- Jordan Thayer  2015-05-15T08:57:44-04:00

module CommonShellProcesses
( make
, makeAProVE
, makeKoaT
, inttrsToITS
, intTRSPathToITSPath
) where
import ShellProcess
import System.Directory()
import System.FilePath
import Data.List

noContext :: String
noContext = "."

-- For Shell Processes which are just sideffecting
noRedirect :: InputRedirection
noRedirect = IOR Nothing Nothing

-- We probably want to call make all whenever we'd normally call make
make :: String -> ShellProcess
make = ShellProcess "make" noRedirect [BareArg "all"]

{- |
   Output Dir -> Jar -> Run APRoVE against Jar, putting output into Dir
-}
makeAProVE :: String -> String -> ShellProcess
makeAProVE outDir jarLoc =
    ShellProcess
    "aprove"
    (IOR Nothing $ Just $ outloc)
    [ArgFlag ((Long "outputDir"),  outDir), BareArg jarLoc] noContext
    where outloc = outDir ++ jarToAprove file
          file = takeFileName jarLoc

-- | Computes filename for an AProVE output bason on a JAR input
jarToAprove :: String -> String
jarToAprove jarLoc =
    aprove
    where revLoc = reverse jarLoc
          aprove = if "raj." `isPrefixOf` revLoc
                   then reverse $ 'e' : 'v' : 'o' : 'r' : 'p' : 'a' :
                        drop (length "raj") revLoc
                   else undefined

-- | Computes filename for a KoaT output based on an ITS input
itsToKoatOut :: String -> String
itsToKoatOut itsLoc =
    koat
    where revLoc = reverse itsLoc
          koat = if "sti." `isPrefixOf` revLoc
                   then reverse $ 't' : 'a' : 'o' : 'k' :
                        drop (length "its") revLoc
                   else undefined

-- | Computes filename for an ITS output based on an INTTRS input
intTRSPathToITSPath :: String -> String
intTRSPathToITSPath inttrsLoc =
    itsLoc
    where revLoc = reverse inttrsLoc
          itsLoc = if "srttni." `isPrefixOf` revLoc
                   then reverse $ 's' : 't' : 'i' :
                        drop (length "srttni") revLoc
                   else undefined

-- | Int TRS Path -> Shell Process running translation against Int TRS
--   Automatically computes output location from input name
inttrsToITS :: String -> ShellProcess
inttrsToITS inttrsLoc =
    ShellProcess
    "intTRStoITS"
    (IOR (Just inttrsLoc) (Just itsLoc))
    []
    noContext
    where itsLoc = intTRSPathToITSPath inttrsLoc

-- | ITS Path -> Shell Process running KoaT against the ITS
--   Automatically computes output location from input name
makeKoaT :: String -> ShellProcess
makeKoaT itsLoc =
    ShellProcess
    "koat"
    (IOR Nothing (Just $ itsToKoatOut itsLoc))
    [BareArg itsLoc] noContext
