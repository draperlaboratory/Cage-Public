-- an example instance
-- Jordan Thayer  2015-05-14T14:18:16-04:00

import ProcessTree
import ShellProcess
import CommonHaskellProcesses
import CommonShellProcesses
import Control.Monad

aproveproc = ShellProcess "aprove" (IOR Nothing Nothing) [BareArg "fib.jar"] "."
transiProc i =
    ShellProcess
    "intTRStoITS"
    (IOR (Just $ "fib.jar-obl-" ++ (show i) ++ ".inttrs")
     (Just $ "fib.jar-obl-" ++ (show i) ++ ".its"))
    []
    "."

koatiProc i =
    ShellProcess
    "koat"
    (IOR Nothing Nothing)
     [BareArg $ "fib.jar-obl-" ++ (show i) ++ ".its"] "."

make2 = Process "make-fib" PreStart (SP $ make ".") []
aprove = Process "aprove-fib" PreStart (SP aproveproc) [make2]
convertEl i = Process
              ("inttrs->its-fib" ++ show i)
              PreStart
              (SP $ transiProc i)
              [aprove]
koatEl i = Process
           ("koat-its-fib" ++ show i)
           PreStart
           (SP $ koatiProc i)
           [convertEl i]
koatGroup sz = Process "koatAll-fib" PreStart NoOP [koatEl i | i <- [1..sz]]

-- The above example has a problem in that make is represented n times, even
-- though it only needs to be done once.  Checking the file system will
-- fix this, but it's unsatisfying as a programmer.

-- putStrLn $ pretty 0 $ nSteps 3 $  koatGroup 3

-- putStrLn $ pretty 0 $ step $ step $ step $ step $ step $ step $ step $ step $ step $  koatGroup 3

-- putStrLn $ prettyPrint $ runTree $ koatGroup 3


testcat =
    ShellProcess
    "cat"
    (IOR Nothing $ Just "cat.out")
    [BareArg "foo.txt"]

testcmd = ShellProcess
          "wc"
          (IOR (Just "cat.out") (Just "out"))
          [BareFlag $ Short 'l']

bar = makeKoatsFromITS "/home/jtt3620/test"
baz = makeTranslations "/home/jtt3620/cage/data/input/INTTRS"
bot = makeKoatsFromINTTRS "/home/jtt3620/cage/data/input/INTTRS"


-- sample invocation
-- system $ shellString testcat `seq` system $ shellString testcmd

-- example, we have a specified directory containing a makefile
-- and an output directory for AProVE to dump things into.
-- then, run translation into koat in parallel.
-- Note that this definitely has a sync point right after the aprove calls.
sampleEndToEnd javaDir aproveOutDir =
    koatGroup { subProcesses = [aprove] }
    where aprove = makeAProVEFromJars javaDir aproveOutDir
          koatGroup = makeKoatsFromINTTRS aproveOutDir

buzz = sampleEndToEnd "/home/jtt3620/cage/data/input/java/fibonacci/"
       "/home/jtt3620/cage/data/input/java/fibonacci/aproveOut/"

-- putStrLn =<< (liftM prettyPrint $ runTree buzz)
