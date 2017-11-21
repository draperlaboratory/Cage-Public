-- TRS Choosers
-- First Example of an enumerable processor
-- There are really a class of processors that fall under this heading,
-- and this bit of code can go through each of them in some order, mostly
-- defined by the order in which lists of options are defined.

-- Also highlights our using nth on NatTuples in order to provide
-- sequencing over infinitely large option choices.
-- We should be able to nest that as often as desired to get orderings
-- over infinite n-ples, not just tuples.
-- Jordan Thayer  2015-04-27T09:36:31-04:00

{-# LANGUAGE InstanceSigs #-}

module OptionsGrammar.TRSChooser
( TRSChoosers(..)
, TRSChooser(..)
) where

import qualified OptionsGrammar.ArgGrammar as Args

data TRSChoosers = Collapsing | Constructor | ContextSensitive | Duplicating
                 | FullStrategy | Ground | Innermost | LeftLinear | Orthoginal
                 | Outermost | Overlay | RightLinear | Wellformed
                   deriving (Show, Eq)

data TRSChooser = TRSChoose { choice :: TRSChoosers
                            , defaultValue :: Args.TRSPred } deriving (Show,Eq)


instance Args.Enumerable TRSChoosers where
    all :: TRSChoosers -> [TRSChoosers]
    all _ = [ Collapsing , Constructor , ContextSensitive , Duplicating
            , FullStrategy , Ground , Innermost , LeftLinear , Orthoginal
            , Outermost , Overlay , RightLinear , Wellformed]
    nth     :: TRSChoosers -> Int -> TRSChoosers
    nth _ i = case i of
                0 -> Collapsing
                1 -> Constructor
                2 -> ContextSensitive
                3 -> Duplicating
                4 -> FullStrategy
                5 -> Ground
                6 -> Innermost
                7 -> LeftLinear
                8 -> Orthoginal
                9 -> Outermost
                10 -> Overlay
                11 -> RightLinear
                12 -> Wellformed
                otherwise -> undefined

instance Args.Enumerable TRSChooser where
    all :: TRSChooser -> [TRSChooser]
    all _ = [TRSChoose x y | x <- Args.all Collapsing,
                             y <- Args.all Args.Strict]
    nth :: TRSChooser -> Int -> TRSChooser
    nth def i =
        TRSChoose
        (Args.nth (choice def) x)
        (Args.nth (defaultValue def) y)
            where (x,y) = Args.nth (0,0) i


