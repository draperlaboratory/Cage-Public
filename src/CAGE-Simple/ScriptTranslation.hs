module ScriptTranslation
( endToEnd
, endToEndm2
)where
import System.Directory
import Data.List
import ShellProcess
import Control.Monad

findSomething :: String -> String -> IO [String]
findSomething ext dir = do
  fileNames <- getDirectoryContents dir
  return $ map ((dir ++ "/") ++) $  filter (isSuffixOf ext) fileNames

findITS :: String -> IO [String]
findITS = findSomething ".its"

findINTTRS :: String -> IO [String]
findINTTRS = findSomething ".inttrs"

findJARs :: String -> IO [String]
findJARs = findSomething ".jar"

intTRSPathToITSPath :: String -> String
intTRSPathToITSPath inttrsLoc =
    itsLoc
    where revLoc = reverse inttrsLoc
          itsLoc = if "srttni." `isPrefixOf` revLoc
                   then reverse $ 's' : 't' : 'i' :
                        drop (length "srttni") revLoc
                   else undefined

itsToKoatOut :: String -> String
itsToKoatOut itsLoc =
    koat
    where revLoc = reverse itsLoc
          koat = if "sti." `isPrefixOf` revLoc
                   then reverse $ 't' : 'a' : 'o' : 'k' :
                        drop (length "srttni") revLoc
                   else undefined

noRedirect :: InputRedirection
noRedirect = IOR Nothing Nothing


inttrsToITS :: String -> ShellProcess
inttrsToITS inttrsLoc =
    ShellProcess "intTRStoITS" (IOR (Just inttrsLoc) (Just itsLoc)) []
    where itsLoc = intTRSPathToITSPath inttrsLoc


makeKoaT :: String -> ShellProcess
makeKoaT itsLoc =
    ShellProcess
    "koat"
    (IOR Nothing (Just $ itsToKoatOut itsLoc))
    [(Bare itsLoc, "")]


makeAProVE :: String -> String -> ShellProcess
makeAProVE outDir jarLoc =
    ShellProcess
    "aprove"
    noRedirect
    [(Long "outputDir", outDir), (Bare jarLoc, "")]

-- calls with a boolean signature, all good, or some bad

aproveCall :: String -> String -> IO Bool
aproveCall jarDir artifactDir = do
  jars <- findJARs jarDir
  let apcCalls = map (makeAProVE artifactDir) jars
  apcSuccess <- mapM invokeSP apcCalls
  return $ not $ or apcSuccess

transCall :: String -> IO Bool
transCall artifactDir = do
  inttrs <- findINTTRS artifactDir
  let transCalls = map inttrsToITS inttrs
  transSuccess <- mapM invokeSP transCalls
  return $ not $ or transSuccess

koatCall :: String -> IO Bool
koatCall artifactDir = do
  itss <- findITS artifactDir
  let koatCalls = map makeKoaT itss
  koatSuccess <- mapM invokeSP koatCalls
  return $ not $ or koatSuccess

{- |
   Simple variant of the end to end call.  Return true if everything
   went as expected. False otherwise.

   End To End means along the CAGE pathway jar -> inttrs -> its -> koat

   endToEnd :: Directory With Jar Files -> Directory Where Output Goes
               -> Success
-}
endToEnd :: String -> String -> IO Bool
endToEnd jarDir artifactDir = do
  aproveSuccess <- aproveCall jarDir artifactDir
  transSuccess <- transCall artifactDir
  koatSuccess <- koatCall artifactDir
  return $ aproveSuccess && transSuccess && koatSuccess

-- calls identifying failing traces

aproveCallm2 :: String -> String -> IO [String]
aproveCallm2 jarDir artifactDir = do
  jars <- findJARs jarDir
  filterM (invokeSP . makeAProVE artifactDir) jars

transCallm2 :: String -> IO [String]
transCallm2 artifactDir = do
  inttrs <- findINTTRS artifactDir
  filterM (invokeSP . inttrsToITS) inttrs

koatCallm2 :: String -> IO [String]
koatCallm2 artifactDir = do
  itss <- findITS artifactDir
  filterM (invokeSP . makeKoaT) itss

{- |
   Informative variant of the end to end call.  Return list of failing inputs
   in a given end to end run.

   End To End means along the CAGE pathway jar -> inttrs -> its -> koat

   endToEnd :: Directory With Jar Files -> Directory Where Output Goes
               -> [Failing Inputs]
-}
endToEndm2 :: String -> String -> IO [String]
endToEndm2 jarDir artifactDir = do
  aproveSuccess <- aproveCallm2 jarDir artifactDir
  transSuccess <- transCallm2 artifactDir
  koatSuccess <- koatCallm2 artifactDir
  return $ aproveSuccess ++ transSuccess ++ koatSuccess
