
module Interface 
    where
import AST (Ident)
import Data.List (concat, intercalate)

data FunIface = FunIface {
      name :: Ident,
      varNames :: [Ident],
      isPure :: Bool,
      secureArgs :: [Ident],
      runTime :: Maybe Polynomial,
      runSpace :: Maybe Polynomial,
      outputSize :: Maybe Polynomial
    } deriving Show


data Monomial = Monomial { coef :: Integer, pows :: [(Ident, Integer)] }

instance Show Monomial where
    show m = show (coef m) ++ "*" ++ concat (map (\(x, y) -> x ++ "^" ++ show y) $ pows m)

data Polynomial = Polynomial { monos :: [Monomial], constant :: Integer }

instance Show Polynomial where
    show p = intercalate " + " (map show $ monos p) ++ " + " ++ show (constant p)

addMono :: Monomial -> Polynomial -> Polynomial
addMono m p = p { monos = m : monos p }
