{

module PolyParser
  ( lexer
  , parse
  ) where

import AST (Ident)
import Interface
import Data.Char
import Data.List
}

%name polyP
%tokentype { Token }
%error { parseError }

%token
     "?"         { TokUnknown }
     "+"         { TokPlus }
     "*"         { TokTimes }
     "-"         { TokNeg }
     "^"         { TokExp }
     int         { TokInt $$ }
     ident       { TokIdent $$ }


%%

MaybePoly : "?" { Nothing }
          | Poly { Just $1 }

Poly : Mono { Polynomial [$1] 0 }
     | Mono "+" Poly { addMono $1 $3 }
     | int  { Polynomial [] $1 }

Mono : int "*" VarPows { Monomial $1 $3 }
     | VarPows         { Monomial 1 $1 }

VarPows : VarPow { [$1] }
        | VarPow "*" VarPows { $1 : $3 }

VarPow : Var { ($1, 1) }
       | Var "^" int { ($1, $3) }

Var : ident { $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse Error"


data Token
     = TokFunName
       | TokUnknown
       | TokPlus  
       | TokTimes  
       | TokNeg    
       | TokExp    
       | TokInt Integer 
       | TokIdent String


lexer :: String -> [Token]
lexer [] = []
lexer  (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('?':cs) = TokUnknown : lexer cs
lexer ('+':cs) = TokPlus    : lexer cs
lexer ('*':cs) = TokTimes   : lexer cs
lexer ('-':cs) = TokNeg     : lexer cs
lexer ('^':cs) = TokExp     : lexer cs


lexNum cs = TokInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlphaNum cs of
      (var,rest)   -> TokIdent var : lexer rest


parse :: String -> Maybe Polynomial
parse = polyP . lexer

}
