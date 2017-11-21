-- A description of the Strategy grammar

{-# LANGUAGE FlexibleInstances, InstanceSigs #-}

module OptionsGrammar.ArgGrammar
( Named(..)
, CLI(..)
, Enumerable(..)
, Toggle(..)
, TRSPred(..)
, BoundInit(..)
, BoundEnrichment(..)
, MatrixCert(..)
, PolyType(..)
, Argument(..)
, IntOpt
, Split
, Select
, NatTuple(..)
-- I'm less certain these belong here
, Processor
, ProcOpt
, Processors
) where

firstNat = 0 -- The spec says nat, but no one agrees on what that means.
type Name = String
type AsCLI = String
type NonEmptyList a = (a, [a])
type IntOpt = Maybe Int
type Processor = String -- just stubbing it in -- I don't know what the type is
type ProcOpt = Maybe Processor
type Processors = NonEmptyList Processor
type NatTuple = (Int , Int)

class Named a where name :: a -> Name

class CLI a where asCLI :: a -> AsCLI

class Enumerable a where
    all :: a -> [a]
    nth :: a -> Int -> a

data Toggle = On | Off deriving (Show, Eq)

data TRSPred = Strict | Weak | Both | Union deriving (Show, Eq)

data BoundInit = Minimal | PerSymbol deriving (Show, Eq)

data BoundEnrichment = Match | Roof | Top deriving (Show, Eq)

-- None is supposed to be nothing, but namespace conflict.
data MatrixCert = Algebraic | Automaton | Triangular | None deriving (Show, Eq)

data PolyType = StronglyLinear | Linear | Simple | SimpleMixed | Quadratic |
         Upto Int deriving (Show, Eq)

data Allow = Add | Mult | Compose
data Split = AnyAll | AnyDPS | AnyRules | AnyStricts | AnyWeaks | AnyFirstCongruence | AnyFirstStrictCongruence | AllAll | AllDPS | AllRules | AllStricts | AllWeaks | AllFirstCongruence | AllFirstStrictCongruence
type Select = Split

-- ignoring types needed by Decompose, SimpPE and WeightGap, as they're the
-- most complicated. We can revisit that later.

data Argument a = Arg
    { argName :: Name
    , defaultValue :: a } deriving Show

instance Enumerable (Argument Int) where
    all :: Argument Int -> [Argument Int]
    all a = [Arg (argName a) i | i <- [firstNat..]]
    nth :: Argument Int -> Int -> Argument Int
    nth a = Arg (argName a)

instance Show a => CLI (Argument a) where
    asCLI :: Argument a -> AsCLI
    asCLI a = ':' : argName a ++ ' ' : show (defaultValue a)

instance Enumerable (Argument Toggle) where
    all :: Argument Toggle -> [Argument Toggle]
    all a = [Arg (argName a) Off , Arg (argName a) On ]
    nth :: Argument Toggle -> Int -> Argument Toggle
    nth a i = Arg (argName a) $
              case i of
                0 -> Off
                1 -> On
                otherwise -> undefined

instance Enumerable (Argument TRSPred) where
    all :: Argument TRSPred -> [Argument TRSPred]
    all a = [Arg (argName a) v | v <- [Strict, Weak, Both, Union]]
    nth :: Argument TRSPred -> Int -> Argument TRSPred
    nth a i = Arg (argName a) $
              case i of
                0         -> Strict
                1         -> Weak
                2         -> Both
                3         -> Union
                otherwise -> undefined

instance Enumerable TRSPred where
    all :: TRSPred -> [TRSPred]
    all _ = [Strict, Weak, Both, Union]
    nth :: TRSPred -> Int -> TRSPred
    nth _ i = case i of
                0 -> Strict
                1 -> Weak
                2 -> Both
                3 -> Union

instance Enumerable (Argument BoundInit) where
    all :: Argument BoundInit -> [Argument BoundInit]
    all a = [Arg (argName a) v | v <- [Minimal, PerSymbol]]
    nth :: Argument BoundInit -> Int -> Argument BoundInit
    nth a i =  Arg (argName a) $
               case i of
                 0         -> Minimal
                 1         -> PerSymbol
                 otherwise -> undefined

instance Enumerable (Argument BoundEnrichment) where
    all :: Argument BoundEnrichment -> [Argument BoundEnrichment]
    all a = [Arg (argName a) v | v <- [Match, Roof, Top]]
    nth :: Argument BoundEnrichment -> Int -> Argument BoundEnrichment
    nth a i =  Arg (argName a) $
               case i of
                 0         -> Match
                 1         -> Roof
                 2         -> Top
                 otherwise -> undefined

instance Enumerable (Argument MatrixCert) where
    all :: Argument MatrixCert -> [Argument MatrixCert]
    all a = [Arg (argName a) v |
             v <- [Algebraic, Automaton, Triangular, None]]
    nth :: Argument MatrixCert -> Int -> Argument MatrixCert
    nth a i =  Arg (argName a) $
               case i of
                 0         -> Algebraic
                 1         -> Automaton
                 2         -> Triangular
                 3         -> None
                 otherwise -> undefined

instance Enumerable (Argument PolyType) where
    all :: Argument PolyType -> [Argument PolyType]
    all a = [Arg (argName a) v |
             v <- [StronglyLinear, Linear, Simple, SimpleMixed, Quadratic] ++
                  [Upto x | x <- [firstNat..]]]
    nth :: Argument PolyType -> Int -> Argument PolyType
    nth a i =  Arg (argName a) $
               case i of
                 0         -> StronglyLinear
                 1         -> Linear
                 2         -> Simple
                 3         -> SimpleMixed
                 4         -> Quadratic
                 otherwize -> Upto i

instance Enumerable NatTuple where
    all :: NatTuple -> [NatTuple]
    all _ = [(x,y) | x <- [1..], y <- [1..]]
    nth :: NatTuple -> Int -> NatTuple
    nth _ i = nthTupleAux i (0,0)

nthTupleAux :: Int -> (Int, Int) -> (Int,Int)
nthTupleAux 0 accum = accum
nthTupleAux i (x,y)
    | x == 0 = nthTupleAux (i - 1) (y + 1, 0)
    | otherwise = nthTupleAux (i - 1) (x - 1, y + 1)
