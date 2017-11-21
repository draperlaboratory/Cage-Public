-- Jordan Thayer  2015-05-04T11:51:57-04:00

module CanonicalIntTRS
( canonize
, nextVariableIndex
, variableIndex
, sameFunction
) where

import AST
import qualified Data.List as DL
import Data.Char

type PrototypeAndRemovable = (Exp , [Int])

-- TODO: Brittle. Should be using a maybe or something here.
variableIndex :: Variable -> Int
variableIndex (Variable s) =
    let asNumber = tail s in
    if any (not . isDigit) asNumber
    then -1
    else if null asNumber
         then -1
         else (read asNumber :: Int)


maximumVariableIndex :: [Variable] -> Int
maximumVariableIndex vs =
    foldl max (-1) (map variableIndex vs)

nextVariableIndex :: [Variable] -> Int
nextVariableIndex vs = 1 + maximumVariableIndex vs

-- | Gives the indexes of repeated arguments.  Doesn't give the first index
--   as, presumably, we don't want to remove that one.
repeatedArguments :: Exp -> [Int]
repeatedArguments (NumExpr _) = []
repeatedArguments (Function _ args) =
    [y | x <- indexes,
         y <- indexes,
         y > x,
         args !! x == args !! y ]
    where indexes = [ 0 .. length args - 1 ]

-- | Drops the specified element from the list
dropEl :: Int -> [a] -> [a]
dropEl i l = take i l ++ drop (i+1) l

-- | Map dropel across a list of indexes to be dropped
dropAll :: [Int] -> [a] -> [a]
dropAll tl l = foldl (flip dropEl) l tl

-- | Find the intersection of eqable lists
intersection :: Eq a => [[a]] -> [a]
intersection lists =
    [x | x <- DL.nub $ concat lists,
         all (elem x) lists]

-- | Give me a list of indexes that can be removed
findDroppableArgs :: [Exp] -> [PrototypeAndRemovable]
findDroppableArgs exps =
    let grouped = DL.groupBy sameFunction $  DL.sort exps in
    map (\ similarExps ->
             (head similarExps,
              intersection (map repeatedArguments similarExps))) grouped

-- | Reduce rules so that functions don't have repeated arguments
reduce :: [PrototypeAndRemovable] -> TRSRule -> TRSRule
reduce removable (TRSRule l r c) =
    TRSRule (reduceExp removable l) (reduceExp removable r) c

-- | Reduce functions so that they don't have repeated arguments
reduceExp :: [PrototypeAndRemovable] -> Exp -> Exp
reduceExp _ n @ (NumExpr _) = n
reduceExp [] f @ (Function _ _) = f
reduceExp ((pt,removable):tl) f @ (Function fname args) =
    if sameFunction pt f
    then Function fname $ dropAll removable args
    else reduceExp tl f


-- | Are two functions the same function (name and arg count)
sameFunction :: Exp -> Exp -> Bool
sameFunction (Function fname1 alist1) (Function fname2 alist2) =
    fname1 == fname2 && length alist1 == length alist2
sameFunction _ _ = False

{- |
   Puts rules into a cannonical form in which repeated arguments have
   been removed.
-}
canonize :: TRSRules -> TRSRules
canonize rlist =
    map (reduce $ findDroppableArgs $ uniqueFunctions rlist) rlist

