module Main
where
import ScriptTranslation

main :: IO Bool
main = do
  putStrLn "Usage : I Expect the absolute path to a directory with jars,"
  putStrLn "        followed by an absolute path to an output directory."
  jarDir <- getLine
  artifactDir <- getLine
  failedArtefacts <- endToEndm2 jarDir artifactDir
  putStrLn "Failing Inputs: "
  print failedArtefacts
  return $ null failedArtefacts

