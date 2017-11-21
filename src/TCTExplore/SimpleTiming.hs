-- Jordan Thayer  2015-04-23T11:08:00-04:00

module SimpleTiming
( TimingResult
, timeIOFn
, timeFn
) where
import System.CPUTime

-- | Simple module for timing functions in haskell. Light weight and inaccurate

data TimingResult a b = TR { result  ::a
                           , seconds :: b } deriving Show

{-|
  Evaluate a function that takes a single argument, return an object
  containing the result of that application and time taken as a number
  of seconds.  Assumes that the function in question returns an
  IO a.
 -}
timeIOFn :: Fractional b => (t -> IO a) -> t -> IO (TimingResult a b)
timeIOFn action arg = do
  startTime <- getCPUTime
  val <- action arg
  finishTime <- getCPUTime
  return $ TR val (fromIntegral (finishTime - startTime) / 1000000000000)

{-|
  Wrapper around timeIOFn for functions that are pure.
-}
timeFn :: Fractional b => (t -> a) -> t -> IO (TimingResult a b)
timeFn f = timeIOFn (\args -> return $! f args)
