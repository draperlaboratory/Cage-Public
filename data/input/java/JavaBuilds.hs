import Example
import Test.Tasty



main = do
  allBuilds <- makeAllJavaBuilds "."
  defaultMain $ testGroup "Java Builds" allBuilds
