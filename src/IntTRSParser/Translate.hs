-- Jordan Thayer  2015-06-26T11:10:07-04:00

module Main where
import IntTRSParser
import IntTRStoITS
import AST
import Pretty

main :: IO ()
main = do
    asString <- getContents
    let inttrs = parse asString
        its = rulesToITRS inttrs
        itsString = endToEnd its
    putStrLn itsString
