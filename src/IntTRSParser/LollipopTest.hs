-- Tester for Lollipop Instances
-- Jordan Thayer  2015-05-19T16:35:06-04:00

module Main (main) where
import IntTRStoITS
import AST
import Test.QuickCheck
import Lollipop

testLollipop size = validateLollipop size size
    where types = size::Int

main = do
  quickCheck testLollipop
