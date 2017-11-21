-- Common Haskell Processes for CAGE system
-- Jordan Thayer  2015-05-15T11:16:41-04:00

module CommonHaskellProcesses
( makeTranslations
, makeKoatsFromITS
, makeKoatsFromINTTRS
, makeAProVEFromJars
) where
import ProcessTree
import ShellProcess
import CommonShellProcesses
import Data.List
import System.Directory
import Control.Monad

-- | extension -> directory -> Files having that extension in directory
findSomething :: String -> String -> IO [String]
findSomething ext dir = do
  fileNames <- getDirectoryContents dir
  return $ map (dir ++) $  filter (isSuffixOf ext) fileNames

-- | extension -> directory -> ITS files in directory
--   Short hand for findSomething ".its"
findITS :: String -> IO [String]
findITS = findSomething ".its"

-- | extension -> directory -> INTTRS files in directory
--   Short hand for findSomething ".inttrs"
findINTTRS :: String -> IO [String]
findINTTRS = findSomething ".inttrs"

-- | extension -> directory -> JAR files in directory
--   Short hand for findSomething ".jar"
findJARs :: String -> IO [String]
findJARs = findSomething ".jar"

-- | diretory -> Process Translating all INTTRS files to ITS files in directory
makeTranslations :: String -> Process
makeTranslations dir =
    Process
    ("TranslateINTTRS-" ++ dir)
    PreStart
    (Mutation myThunk) -- Process -> IO Process
    []
    where myThunk base = liftM (Process
                                      ("TranslateINTTRS-" ++ dir)
                                      PreStart
                                      NoOP) $ makeSubs base
          makeSubs base = liftM2
                           (zipWith $ makeITSProcOfShell (subProcesses base))
                           translateShells translateInputs
          translateShells = liftM (map inttrsToITS) translateInputs
          translateInputs = findINTTRS dir

-- | directory -> Process calling KoaT on all ITS files in directory
makeKoatsFromITS :: String -> Process
makeKoatsFromITS dir =
    Process
    ("BuildKoats-" ++ dir)
    PreStart
    (Mutation myThunk)
    []
    where myThunk base = liftM (Process
                                ("RunKoat-" ++ dir)
                                PreStart
                                NoOP) $ makeSubs base
          makeSubs base = liftM2 (zipWith $ makeKoaTOfShell (subProcesses base))
                           koatShells koatInputs
          koatShells = liftM (map makeKoaT) koatInputs
          koatInputs = findITS dir

{- |
   directory -> Process calling KoaT on translated INTTRS files in directory

   You want this instead of just the previous two calls stitched together
   because in the previous case, the koat calls will be blocked against all of
   the translate calls.  Here, each koat call will only block on its
   translation output, which is the behavior that you probably want.
 -}
makeKoatsFromINTTRS :: String -> Process
makeKoatsFromINTTRS dir =
    Process
    ("KoatGroup-" ++ dir)
    PreStart
    (Mutation myThunk)
    []
    where
      myThunk base =
          liftM
          (Process
           ("KoatGroup-" ++ dir)
           PreStart
           NoOP)
          $ makeSubs base
      makeSubs base = liftM (map $ makeChain base) translateInputs
      makeChain rootProc intTRS =
          let transSP = SP $ inttrsToITS intTRS
              koatSP = SP $ makeKoaT (intTRSPathToITSPath intTRS) in
          makeKoatSP (subProcesses rootProc) koatSP transSP intTRS
      makeKoatSP sps koatSP transSP path =
          Process ("KoatProc-" ++ path)
                  PreStart koatSP [makeTProc sps transSP path]
      makeTProc sps transSP path =
          Process ("TransProc"++path) PreStart transSP sps
      translateInputs = findINTTRS dir
{- |
   JAR Directory -> Output Directory ->
     Process running AProVE on each JAR in in JAR Directory, using output dir

   Structured s.t. the AProVE calls can happen in parallel -}
makeAProVEFromJars :: String -> String -> Process
makeAProVEFromJars inDir outDir =
    Process ("Aprove-" ++ inDir)
    PreStart
    (Mutation myThunk)
   []
    where
      myThunk base = liftM
                     (Process ("AProVEGroup-" ++ inDir)
                              PreStart
                              NoOP) $ makeSubs base
      makeSubs base = liftM2 (zipWith (aproveCall base))
                      aproveShells aproveInputs
      aproveCall base sp jp =
          Process
          ("GeneratedAprove-" ++ jp)
          PreStart
          (SP sp)
          (subProcesses base)
      aproveShells = liftM (map $ makeAProVE outDir) aproveInputs
      aproveInputs = findJARs inDir

-- helper for generating translation processes, not exposed
makeITSProcOfShell :: [Process] -> ShellProcess -> String -> Process
makeITSProcOfShell subs shell dir =
    Process ("intTRSToITSGenerated-"++dir) PreStart (SP shell) subs

-- helper for generating koat processes, not exposed
makeKoaTOfShell :: [Process] -> ShellProcess -> String -> Process
makeKoaTOfShell subs shell dir =
    Process ("KoATGenerated-"++dir) PreStart (SP shell) subs
