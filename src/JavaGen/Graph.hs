{-# LANGUAGE FlexibleInstances #-}

module Graph
( makeFunction
, nextVariable
, Exp(..)
, Node(..)
, Edge(..)
, NumExpr(..)
, BoolExp(..)
, NumConstr(..)
, Constraints(..)
, Graph(..)
, Variable(..)
, toJava
) where

import System.Random
import Data.List (intersperse)

type Graph = [(Node , [Edge])]

data Node = Node Exp deriving Show
data Edge = Edge { origin      :: Node
                 , destination :: Node
                 , argTrans    :: [Exp]
                 , guard       ::  Constraints } deriving Show

data Constraints =
   Constraints BoolExp
 | NoConstraints deriving (Show, Eq)

data BoolExp =
       And NumConstr BoolExp
     | Or NumConstr BoolExp
     | NumConstr NumConstr deriving (Show, Eq)

data NumConstr =
       LocalEq NumExpr NumExpr
     | LocalGTE NumExpr NumExpr
     | LocalGT NumExpr NumExpr
     | LocalLTE NumExpr NumExpr
     | LocalLT NumExpr NumExpr deriving (Show, Eq)

data NumExpr =
    Times NumExpr NumExpr
  | Plus NumExpr NumExpr
  | Int Int
  | Var Variable deriving (Show, Eq)

newtype Variable = Variable String deriving (Show, Eq, Ord)

data Exp =
    Function {fName :: Ident,
              argList :: [Exp] }
  | NumExpr NumExpr deriving (Show, Eq)

type Ident = String


nextIdent :: Int -> String
nextIdent i = "function" ++ show i

nextVariable :: Int -> Variable
nextVariable i = Variable $ 'x' : show i

makeFunction :: Int -> Int -> Exp
makeFunction numArgs fIndex =
    Function name args
    where name = nextIdent fIndex
          args = map (NumExpr . Var . nextVariable) [1..numArgs]


-- | Reduce arglist to requisite number by some process
reduceArgs :: Int -> [Exp] -> [Exp]
reduceArgs toRem argList = undefined

-- | Extend arglist to some requisite number by some process
extendArgs :: Int -> [Exp] -> [Exp]
extendArgs toAdd argList = undefined

-- | Pad / shrink arguments out / down to the correct size
modArgs :: Int -> [Exp] -> [Exp]
modArgs size argList
 | size == length argList = argList
 | size < length argList = reduceArgs ((length argList) - size) argList
 | size > length argList = extendArgs (size - length argList) argList

-- | Give me a list of count random integers between 0 and max, using a seed
randomPositiveInts :: Int -> Int -> Int -> [Int]
randomPositiveInts seed max count =
    randomPositiveIntsAux gen max count
    where gen = mkStdGen seed

randomPositiveIntsAux :: StdGen -> Int -> Int -> [Int]
randomPositiveIntsAux gen max rem
 | rem == 0 = []
 | otherwise = mod (abs val) max : randomPositiveIntsAux gen' max (rem - 1)
     where (val, gen') = next gen


-- | Get a list of numFunctions random functions of maximum arity maxArity, using a seed
randomFunctionSignatures :: Int -> Int -> Int -> [Exp]
randomFunctionSignatures seed maxArity numFunctions =
    zipWith makeFunction fInds argCounts
    where argCounts = randomPositiveInts seed maxArity numFunctions
          fInds = [1..numFunctions]

tabs :: Int -> String
tabs indent = replicate indent '\t'

printLn :: Int -> String -> [Exp] -> String
printLn indent name args =
    tabs (indent + 1) ++
    "System.out.println(\"" ++ name ++ " \" + " ++
    concat (intersperse " + \" \" + " $ map (toJava False 0) args) ++ ");\n"

class Javable a where
    toJava :: Bool -> Int -> a -> String

instance Javable NumExpr where
    toJava _ _ (Int i) = show i
    toJava _ i (Var v) = toJava False i v
    toJava _ i (Plus ne1 ne2) = toJava False i ne1 ++ " + " ++ toJava False i ne2
    toJava _ i (Times ne1 ne2) = toJava False i ne1 ++ " * " ++ toJava False i ne2

instance Javable Variable where
    toJava _ _ (Variable s) = s

instance Javable NumConstr where
    toJava _ i (LocalEq ne1 ne2) =
        toJava False i ne1 ++ " == " ++ toJava False i ne2
    toJava _ i (LocalGTE ne1 ne2) =
        toJava False i ne1 ++ " >= " ++ toJava False i ne2
    toJava _ i (LocalLTE ne1 ne2) =
        toJava False i ne1 ++ " <= " ++ toJava False i ne2
    toJava _ i (LocalGT ne1 ne2) =
        toJava False i ne1 ++ " > " ++ toJava False i ne2
    toJava _ i (LocalLT ne1 ne2) =
        toJava False i ne1 ++ " < " ++ toJava False i ne2

instance Javable BoolExp where
    toJava hr i (NumConstr nc) = toJava hr i nc
    toJava _ i (And nc be) = toJava False i nc ++ " && " ++ toJava False i be
    toJava _ i (Or nc be) = toJava False i nc ++ " || " ++ toJava False i be

instance Javable Constraints where
    toJava _ i (Constraints be) = toJava False i be
    toJava _ _ NoConstraints = "true"

instance Javable Exp where
    toJava _ _ (Function fname arglist) =
        fname ++ '(' : concat (intersperse ", " $
                               map (toJava False 0) arglist) ++ ")"
    toJava _ i (NumExpr ne) = toJava False i ne

instance Javable Edge where
    toJava _ indent (Edge _ (Node (Function destFname _)) args NoConstraints) =
        replicate indent '\t' ++
        "return " ++ toJava False 0 (Function destFname args) ++ ";\n"

    toJava _ indent (Edge _ (Node (NumExpr ne)) args NoConstraints) =
        replicate indent '\t' ++
        "return " ++ toJava False 0 ne ++ ";\n"

    toJava _ indent (Edge _ (Node (Function destFname _)) args guard) =
        tabs indent ++
        "if (" ++ toJava False 0 guard ++ "){\n" ++
        tabs (indent + 1) ++
        "return " ++ toJava False 0 (Function destFname args) ++ ";\n" ++
        tabs indent ++ "}"

    toJava _ indent (Edge _ (Node (NumExpr ne)) args guard) =
        replicate indent '\t' ++
        "if (" ++ toJava False 0 guard ++ "){\n" ++
        tabs (indent + 1) ++
        "return " ++ toJava False 0 ne ++ ";\n" ++
        tabs indent ++ "}"

instance Javable (Node, [Edge]) where
    toJava human indent (Node f @ (Function name args), edgeList) =
        tabs indent ++
        "public int " ++ name ++ "(" ++ argString ++ ")" ++ "{\n" ++
        (\hr -> if hr
                then printLn indent name args
                else "") human ++ -- AProVE can't handle these
        concat (intersperse ['\n'] $ map (toJava False $ indent + 1) edgeList)
        ++ "\n" ++ tabs indent ++ "}"
        where argString = concat $ intersperse ", " $
                                map (\ x -> "int " ++ toJava False 0 x) args

instance Javable Graph where
    toJava human indent elements =
        concat (intersperse ['\n', '\n'] $
                            map (toJava human $ indent + 1) elements)
