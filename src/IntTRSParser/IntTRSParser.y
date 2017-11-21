{
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module IntTRSParser
  ( inttrs
  , lexer
  , parse
  ) where
import AST
import Data.Char
import qualified Data.List as DL
import qualified Data.Text as DT
}

%name inttrs
%tokentype { Token }
%error { parseError }

%token
        '('     { TokenOP }
        ')'     { TokenCP }
        '->'    { TokenArrow }
        '['     { TokenOS }
        ']'     { TokenCS }
        ','     { TokenComma }
        '/\\'   { TokenAnd }
        '\\/'   { TokenOr }
        '>='    { TokenLocalGTE }
        '<='    { TokenLocalLTE }
        '>'     { TokenLocalGT }
        '<'     { TokenLocalLT }
        '='     { TokenLocalEq }
        '+'     { TokenPlus }
        '*'     { TokenTimes }
        '-'     { TokenNeg }
        int     { TokenInt $$ }
        ident   { TokenIdent $$ }
%%

Rule        : Left '->' Right Constraints { TRSRule $1 $3 $4 }
            | Left '->' Right             { TRSRule $1 $3 NoConstraints }

Left        : Exp                         { $1 }

Right       : Exp                         { $1 }

ExpList     : Exp ',' ExpList             { $1 : $3 }
            | Exp                         { [$1] }

Exp         : ident '(' ExpList ')'         { Function $1 $3 }
            | NumExpr                     { NumExpr $1 }

Constraints : '[' BoolExp ']'             { Constraints $2 }
            | '['']'                      { NoConstraints }

BoolExp     : NumConstr '/\\' BoolExp     { And $1 $3 }
            | NumConstr '\\/' BoolExp     { error "Unsupported operator \\/"  }
            | NumConstr                   { NumConstr $1 }

NumConstr   : NumExpr '>=' NumExpr        { LocalGTE $1 $3 }
            | NumExpr '>' NumExpr         { LocalGT $1 $3 }
            | NumExpr '<=' NumExpr        { LocalLTE $1 $3 }
            | NumExpr '<' NumExpr         { LocalLT $1 $3 }
            | NumExpr '=' NumExpr         { LocalEq $1 $3 }

NumExpr     : Factor '*' NumExpr          { Times $1 $3 }
            | Factor '+' NumExpr          { Plus $1 $3 }
            | '-' NumExpr                 { negateNE $2 }
            | Factor                      { $1 }

Factor      : int                         { Int $1 }
            | ident                       { Var (Variable $1) }
            | '(' NumExpr ')'             { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse Error"


data Token
     = TokenOS
     | TokenCS
     | TokenOP
     | TokenCP
     | TokenComma
     | TokenArrow
     | TokenIdent String
     | TokenInt Int
     | TokenAnd
     | TokenOr
     | TokenLocalGTE
     | TokenLocalLTE
     | TokenLocalGT
     | TokenLocalLT
     | TokenLocalEq
     | TokenPlus
     | TokenNeg
     | TokenTimes deriving (Show, Eq)



lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('[':cs) = TokenOS : lexer cs
lexer (']':cs) = TokenCS : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('>':'=':cs) = TokenLocalGTE : lexer cs
lexer ('<':'=':cs) = TokenLocalLTE : lexer cs
lexer ('>':cs) = TokenLocalGT : lexer cs
lexer ('<':cs) = TokenLocalLT : lexer cs
lexer ('=':cs) = TokenLocalEq : lexer cs
lexer ('\\':'/':cs) = TokenOr : lexer cs
lexer ('/':'\\':cs) = TokenAnd : lexer cs
lexer ('-':cs) = TokenNeg : lexer cs
varHalt :: Char -> Bool
varHalt ' ' = False
varHalt '[' = False
varHalt ']' = False
varHalt '(' = False
varHalt ')' = False
varHalt ',' = False
varHalt '+' = False
varHalt '-' = False
varHalt '>' = False
varHalt '<' = False
varHalt '=' = False
varHalt '*' = False
varHalt '\n' = False
varHalt _   = True

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span varHalt cs of
      (var,rest)   -> TokenIdent var : lexer rest

-- | Wrapper around parsing, probably need to move this back to IntTRSParser
parse :: String -> TRSRules
parse "" = []
parse s =
  map (\el -> inttrs (lexer $ el)) $
  filter (/= "") $ map DT.unpack $ DT.split (== '\n') $ DT.pack s
}
