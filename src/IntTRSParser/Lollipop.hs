-- Generate Examples of 'Lollipop' style graphs, which can be difficult
-- to find an entry point for

-- Jordan Thayer  2015-05-13T13:11:06-04:00

module Lollipop ( validateLollipop )  where
import IntTRStoITS
import AST

makeLink :: Int -> ITSRule
makeLink i =
    ITSRule
    (SFun ("Chain" ++ show i) [Variable "x0"])
    (SFun ("Chain" ++ show (i+1)) [Variable "x0"])
    NoConstraints

makeCycleEl :: Int -> Int -> ITSRule
makeCycleEl this max
    | this == max =
        ITSRule
        (SFun ("Loop" ++ show this) [Variable "x0"])
        (SFun "Loop0" [Variable "x0"])
        NoConstraints
    | otherwise =
        ITSRule
        (SFun ("Loop" ++ show this) [Variable "x0"])
        (SFun ("Loop" ++ show (this+1)) [Variable "x0"])
        NoConstraints

makeCycle :: Int -> ITSRules
makeCycle length = [ makeCycleEl i (length - 1) | i <- [ 0 .. length - 1 ]]

-- | Construct an Integer TRS. with a lollipop shape
makeInstance :: Int -> Int -> [ITSRule]
makeInstance chainLeng cycleLeng =
    chain ++ attach : loop
    where chain = [ makeLink i | i <- [ 0 .. chainLeng - 1]]
          loop = makeCycle cycleLeng
          attach = ITSRule
                   (SFun ("Chain" ++ show chainLeng)
                                 [Variable "x0"])
                   (SFun "Loop0" [Variable "x0"])
                   NoConstraints

validateLollipop :: Int -> Int -> Bool
validateLollipop chainLeng cycleLeng =
    if chainLeng <= 0 || cycleLeng <= 0 then True
    else correct == findEntryPoint inst
    where inst = makeInstance chainLeng cycleLeng
          correct = SFun "Chain0" [Variable "x0"]
