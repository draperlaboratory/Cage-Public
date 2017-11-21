-- Jordan Thayer  2015-05-06T09:01:42-04:00

module Main
where
import IntTRSParser ()
import IntTRStoITS ()
import InterfaceParser

import Dep

import AST (ITRS)

-- test :: [DGraph] -> Maybe DGraph
-- test (g:_) = composeGraph g g

findLeaks :: ITRS -> Bool
findLeaks rew = let gs = mkClosure $ mkDGraphs rew
                    pos = [Position 0]
                in
                  leaks rew pos gs


main :: IO ()
-- main = getContents >>= putStrLn . show . findLeaks . rulesToITRS . parse
main = getContents >>= putStrLn . show . ifaceFromJSON
