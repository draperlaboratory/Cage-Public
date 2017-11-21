{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- Data and Class types representing IntTRS / ITS instances
-- Jordan Thayer  2015-05-12T09:06:40-04:00

module AST
(TRSRule(..)
, TRSRules
, ITSRule(..)
, ITSRules
, Constraints(..)
, Exp(..)
, SExp(..)
, BoolExp(..)
, NumConstr(..)
, NumExpr(..)
, ITRS(..)
, Ident
, Variable(..)
, uniqueFunctions
, uniqueVars
, negateNE
, concatConstr
, ruleHead
) where
import qualified Data.List as DL

data TRSRule = TRSRule { trsLeft :: Exp, trsRight :: Exp, trsConstr :: Constraints }
               deriving Show


type TRSRules = [TRSRule]

data ITSRule = ITSRule { itsLeft :: SExp, itsRight :: SExp, itsConstr :: Constraints }

type ITSRules = [ITSRule]

data Constraints =
   Constraints BoolExp
 | NoConstraints deriving (Show, Eq)

type Ident = String

newtype Variable = Variable String
    deriving (Show, Eq, Ord)

data SExp = SFun { sFunName :: Ident, sArgList :: [Variable] } | SVar Variable
          deriving (Show, Eq)

data Exp =
    Function {fName :: Ident,
              argList :: [Exp] }
  | NumExpr NumExpr deriving (Show, Eq)

data BoolExp =
       And NumConstr BoolExp
-- disjunctive constraints are unsupported for now
--     | Or NumConstr BoolExp
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

data ITRS = ITRS { entryPoint :: SExp,
                   varList :: [Variable],
                   ruleList :: ITSRules }


negateNE :: NumExpr -> NumExpr
negateNE (Int i) = Int (-i)
negateNE (Plus (Int i) t) = Plus (Int (-i)) t
negateNE (Times (Int i) t) = Times (Int (-i)) t
negateNE (Var s) = Times (Int (-1)) (Var s)
negateNE _ = undefined

-- | Concatenate two constraints, taking taking the conjunction
concatConstr :: Constraints -> Constraints -> Constraints
concatConstr NoConstraints m = m
concatConstr m NoConstraints = m
concatConstr (Constraints cs) (Constraints ds) = Constraints (concatBoolExp cs ds)
    where concatBoolExp (NumConstr c) ds = And c ds
          concatBoolExp (And c cs) ds = And c (concatBoolExp cs ds)


ruleHead :: ITSRule -> Ident
ruleHead r =
    case itsLeft r of
      SFun f _ -> f
      _ -> error "ruleHead: ill formed rule!"

-- | 'uniqueVars' collects the free variables of the datatype a
-- and removes duplicates
class HasVariables a where
    uniqueVars :: a -> [Variable]

instance HasVariables NumExpr where
    uniqueVars (Times e1 e2) = DL.nub $ uniqueVars e1 ++ uniqueVars e2
    uniqueVars (Plus e1 e2) = DL.nub $ uniqueVars e1 ++ uniqueVars e2
    uniqueVars (Int _) = []
    uniqueVars (Var s) = [s]

instance HasVariables NumConstr where
    uniqueVars (LocalGTE e1 e2) = DL.nub $ uniqueVars e1 ++ uniqueVars e2
    uniqueVars (LocalGT e1 e2) = DL.nub $ uniqueVars e1 ++ uniqueVars e2
    uniqueVars (LocalLTE e1 e2) = DL.nub $ uniqueVars e1 ++ uniqueVars e2
    uniqueVars (LocalLT e1 e2) = DL.nub $ uniqueVars e1 ++ uniqueVars e2
    uniqueVars (LocalEq e1 e2) = DL.nub $ uniqueVars e1 ++ uniqueVars e2

instance HasVariables BoolExp where
    uniqueVars (And e1 e2) = DL.nub $ uniqueVars e1 ++ uniqueVars e2
    uniqueVars (NumConstr e1) = DL.nub $ uniqueVars e1

instance HasVariables Exp where
    -- does fname need to be bound as a new variable here?
    uniqueVars (Function _ explist) = DL.nub $ concatMap uniqueVars explist
    uniqueVars (NumExpr e1) = DL.nub $ uniqueVars e1

instance HasVariables SExp where
    uniqueVars (SFun _ vs) = DL.nub vs
    uniqueVars (SVar v) = [v]

instance Ord Exp where
    (Function f1 a1) `compare` (Function f2 a2) =
        if f1 == f2
        then length a1 `compare` length a2
        else f1 `compare` f2
    (Function _ _) `compare` (NumExpr _) = GT
    (NumExpr _) `compare` _ = LT

instance HasVariables Constraints where
    uniqueVars (Constraints e1) = DL.nub $ uniqueVars e1
    uniqueVars NoConstraints = []

instance HasVariables TRSRule where
    uniqueVars (TRSRule l r c) =
        DL.nub $ uniqueVars l ++ uniqueVars r ++ uniqueVars c

instance HasVariables ITSRule where
    uniqueVars (ITSRule l r c) =
        DL.nub $ uniqueVars l ++ uniqueVars r ++ uniqueVars c

instance HasVariables a => HasVariables [a] where
    uniqueVars as =
        DL.nub $ concatMap uniqueVars as


class HasFunctions a where
    uniqueFunctions :: a -> [Exp]
    uniqueFunctionNames :: a -> [String]

instance HasFunctions Exp where
    uniqueFunctions f @ (Function _ args) =
        DL.nub $ f : concatMap uniqueFunctions args
    uniqueFunctions (NumExpr _) = []
    uniqueFunctionNames (Function name args) =
        DL.nub $ name : concatMap uniqueFunctionNames args
    uniqueFunctionNames (NumExpr _) = []

instance HasFunctions TRSRule where
    uniqueFunctions (TRSRule l r _) =
        DL.nub $ uniqueFunctions l ++ uniqueFunctions r
    uniqueFunctionNames (TRSRule l r _) =
        DL.nub $ uniqueFunctionNames l ++ uniqueFunctionNames r

instance HasFunctions TRSRules where
    uniqueFunctions rlist =
        DL.nub $ concatMap uniqueFunctions rlist
    uniqueFunctionNames rlist =
        DL.nub $ concatMap uniqueFunctionNames rlist
