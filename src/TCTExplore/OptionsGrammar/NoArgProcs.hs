-- A collection of Processors containing no Arguments
-- Jordan Thayer  2015-04-27T10:53:13-04:00

{-# LANGUAGE InstanceSigs #-}

module OptionsGrammar.NoArgProcs
    ( Trivial(..)
    , DC2011(..)
    , RC2011(..)
) where

import qualified OptionsGrammar.ArgGrammar as Args

-- Trivial Processors
data Trivial = Open | Empty | Fail | Success

-- Powerful Strategies from Competitions
data DC2011 = DC2011
data RC2011 = RC2011

instance Args.Enumerable Trivial where
    all :: Trivial -> [Trivial]
    all _ = [Open, Empty, Fail, Success]
    nth :: Trivial -> Int -> Trivial
    nth _ i = case i of
                0 -> Open
                1 -> Empty
                2 -> Fail
                3 -> Success
                otherwise -> undefined
