import Example
import Test.Tasty

main = do
   allAproves <- makeAllAproveRuns "."
   defaultMain $ testGroup "All Aprove Runs" allAproves
