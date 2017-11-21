-- Pretty Print the TRS Representation
-- Jordan Thayer  2015-05-05T15:53:35-04:00
-- The right way to do this is with haskell's pretty printer, but it's
-- complicated, and I want to slam out something that can demonstrate the system
-- works
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Pretty (pretty) where
import AST
import Data.List (intersperse)

class Printable a where
    pretty :: a -> String

instance Printable Variable where
    pretty (Variable s) = s

instance Printable NumExpr where
    pretty (Var v) = pretty v
    pretty (Int i) = show i
    pretty (Plus sa1 sa2) = pretty sa1 ++ " + " ++ pretty sa2
    pretty (Times f1 f2) = pretty f1 ++ " * " ++ pretty f2

instance Printable NumConstr where
    pretty (LocalGTE ne1 ne2) = pretty ne1 ++ " >= " ++ pretty ne2
    pretty (LocalGT ne1 ne2) = pretty ne1 ++ " > " ++ pretty ne2
    pretty (LocalLTE ne1 ne2) = pretty ne1 ++ " <= " ++ pretty ne2
    pretty (LocalLT ne1 ne2) = pretty ne1 ++ " < " ++ pretty ne2
    pretty (LocalEq ne1 ne2) = pretty ne1 ++ " = " ++ pretty ne2

instance Printable BoolExp where
    pretty (And nc be) = pretty nc ++ " && " ++ pretty be
    pretty (NumConstr nc) = pretty nc

instance Printable Exp where
    pretty (NumExpr ne) = pretty ne
    pretty (Function fname arglist) =
        fname ++ "("  ++ argString ++ ")"
        where delim = ", "
              argStringRaw = concatMap (\ e -> pretty e ++ delim) arglist
              argString = take (length argStringRaw - length delim) argStringRaw

instance Printable SExp where
    pretty (SFun f args) = f ++ "(" ++ concat (intersperse "," $ map pretty args) ++ ")"
    pretty (SVar x) = pretty x

instance Printable Constraints where
    pretty NoConstraints = ""
    pretty (Constraints be) = " :|: " ++ pretty be

instance Printable TRSRule where
    pretty (TRSRule l r c) = pretty l ++ " -> " ++ pretty r ++ pretty c

instance Printable ITSRule where
    pretty (ITSRule l r c) = pretty l ++ " -> " ++ pretty r ++ pretty c

instance Printable ITSRules where
    pretty [] = ""
    pretty (r:tl) = pretty r ++ '\n' : pretty tl

instance Printable [Variable] where
    pretty = concat . intersperse " " . map pretty
