-- Jordan Thayer  2015-05-05T11:52:01-04:00
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module IntTRStoITS
where
import AST
import CanonicalIntTRS
import Pretty

type State a = (Int, Sigma a)
type Binding a = (Variable, a)
type Sigma a = [Binding a]

data Edge = Edge SExp SExp deriving Show

-- | Returns true if the two expression are function applications
-- of the same function
sameHead :: SExp -> SExp -> Bool
sameHead (SFun f _) (SFun g _) = f == g
sameHead _ _ = error "sameHead: undefined arguments!"

instance Eq Edge where
    (Edge a b) == (Edge c d) = sameHead a c && sameHead b d

class Substable a b where
    subst :: Sigma a -> b -> b

instance Substable Variable Variable where
    subst ((x, y):tl) v =
        if x == v
        then y
        else subst tl v
    subst _ v = v

instance Substable Variable NumExpr where
    subst [] anything = anything
    subst sigma (Times ne1 ne2) = Times (subst sigma ne1) (subst sigma ne2)
    subst sigma (Plus ne1 ne2) = Plus (subst sigma ne1) (subst sigma ne2)
    subst _ i @ (Int _) = i
    subst sigma (Var s) = Var $ subst sigma s


instance Substable Variable NumConstr where
    subst [] anything = anything
    subst sigma (LocalGTE ne1 ne2) =
        LocalGTE (subst sigma ne1) (subst sigma ne2)
    subst sigma (LocalGT ne1 ne2) =
        LocalGT (subst sigma ne1) (subst sigma ne2)
    subst sigma (LocalLTE ne1 ne2) =
        LocalLTE (subst sigma ne1) (subst sigma ne2)
    subst sigma (LocalLT ne1 ne2) =
        LocalLT (subst sigma ne1) (subst sigma ne2)
    subst sigma (LocalEq ne1 ne2) =
        LocalEq (subst sigma ne1) (subst sigma ne2)

instance Substable Variable BoolExp where
    subst [] anything = anything
    subst sigma (And nc be) = And (subst sigma nc) (subst sigma be)
    subst sigma (NumConstr nc) = NumConstr $ subst sigma nc

instance Substable Variable Constraints where
    subst _ NoConstraints = NoConstraints
    subst [] anything = anything
    subst sigma (Constraints be) = Constraints $ subst sigma be

instance Substable Variable SExp where
    subst sigma (SFun f args) = SFun f (map (subst sigma) args)
    subst sigma (SVar v) = SVar $ subst sigma v


instance Substable Variable ITSRule where
    subst sigma (ITSRule l r cs) = ITSRule (subst sigma l) (subst sigma r) (subst sigma cs)

instance Substable Variable ITSRules where
    subst sigma = map (subst sigma)

-- | Returns the arity of a function, and 0 if the expression
-- is not a function
arity :: SExp -> Int
arity (SFun _ alist) = length alist
arity _ = 0

-- Useful for setting all rules to max arity, which koat requires.
-- | Returns is the largest arity for a set of rules.
maxArity :: ITSRules -> Int
maxArity [] = 0
maxArity (ITSRule l r _ : tl) =
    max localMax $ maxArity tl
    where localMax = max (arity l) (arity r)

-- | Rename variables in a rule s.t. they are indexed from 0.
--  Thus
--  > f(x17,x2,x247) = f(x0, x1, x2)
leftShiftVars :: ITSRule -> ITSRule
leftShiftVars rule =
    subst (zipWith (\ s d -> (s, d)) src dst) rule
    where src = uniqueVars rule
          dst = map freshVariable [0..length src - 1]

-- | Give me a variable that's not used in the rule set
freshVariable :: Int -> Variable
freshVariable i = Variable $ 'x' : show i

{- |
   Translate performs the following transformations:
     replaces repeated variables with functions with new variables,
     generating a constraint as well. so f(x,x) -> g(x) becomes
     f(x0,x1) -> g(x0) :|: x0 = x1

     replaces arguments of the form x + 1 / x * 1 / x * y with a
     fresh variable, and adds the constraint that the new variable
     is equal to the previous computation

     replaces arguments of the form 1, 0, etc with a fresh variable,
     and adds the constraint that the new variable is equal to the integer

     replaces functional arguments with a fresh variable, no constraints are
     added.
 -}
translateArgs :: Int -> [Variable] -> [Exp] -> ([NumConstr] , [Variable])
translateArgs _ _ [] = ([], [])
translateArgs ind seen ((NumExpr (Var v)):tl) =
    if v `elem` seen
    then let (equivPairs, expAccum) = translateArgs (ind + 1) seen tl in
         let fresh = freshVariable ind in
         (LocalEq (Var v) (Var fresh) : equivPairs, fresh : expAccum)
    else let (equivPairs, expAccum) = translateArgs ind (v:seen) tl in
         (equivPairs, v : expAccum)
translateArgs ind seen (NumExpr i : tl) =
    (LocalEq (Var fresh) i : equivPairs, fresh : expAccum)
        where (equivPairs, expAccum) = translateArgs (ind + 1) seen tl
              fresh = freshVariable ind
translateArgs ind seen (Function _ _ : tl) =
    (equivPairs, hd : expAccum)
    where hd = freshVariable ind
          (equivPairs, expAccum) = translateArgs (ind + 1) seen tl

{-| Translates an Exp to a SExp and some additional constraints
 -}
translateFn :: Int -> Exp -> ([NumConstr], SExp)
translateFn ind (Function fname alist) =
    (newConstraints, SFun fname alist')
    where (newConstraints, alist')= translateArgs ind [] alist
translateFn _ (NumExpr (Var v)) = ([], SVar v)
translateFn ind (NumExpr e) = ([LocalEq (Var fresh) e], SVar fresh)
    where fresh = freshVariable ind

{-
   Takes a list of numeric constraints and an inital constraints value,
   and returns a Boolean Expression
   It's for updating constraints on a rule as a result of the above rewrites.
 -}
toConstraintsAux :: [NumConstr] -> Constraints -> BoolExp
toConstraintsAux [] _ = undefined
toConstraintsAux [singleton] NoConstraints = NumConstr singleton
toConstraintsAux [singleton] (Constraints be) = And singleton be
toConstraintsAux (hd:tl) baseConst = And hd $ toConstraintsAux tl baseConst

-- | Stitches new numeric constraints onto existing constraint set
toConstraints :: [NumConstr] -> Constraints -> Constraints
toConstraints [] baseCs = baseCs
toConstraints ncs baseCs = Constraints $ toConstraintsAux ncs baseCs

-- | Applies translateFn to the lhs and rhs of a rule
translate :: TRSRule -> ITSRule
translate rule @ (TRSRule l r c) =
    ITSRule l' r' $ toConstraints newcs c
    where (lc, l') = translateFn ind l
          (rc, r') = translateFn (ind + length lc) r
          newcs = lc ++ rc
          ind = nextVariableIndex (uniqueVars rule)
{- |
   Stitches the [Exp] onto the end of a functions argument list.
   Raises an error if called on a variable
 -}
addArgs :: [Variable] -> SExp -> SExp
addArgs [] e = e
addArgs additional (SFun fname alist) = SFun fname $ additional ++ alist
addArgs _ _ = error "addArgs: unexpected second argument"

{- |
   Pads out a function so that it has at least Int arity. If the function
   already has Int arity or more, this is the identity.
 -}
padToArity :: Int -> ITSRule -> ITSRule
padToArity ma (rule @ (ITSRule l r c)) =
    ITSRule (addArgs newLeft l) (addArgs newRight r) c
    where ind = nextVariableIndex (uniqueVars rule)
          la = ma - arity l
          ra = ma - arity r
          newLeft = [freshVariable $ ind + i |
                     i <- [0 .. la - 1]]
          newRight = [freshVariable $ ind + la + i |
                      i <- [0 .. ra - 1]]

-- | Maps padToArity using the maximum arity of the rules
padToMaxArity :: ITSRules -> ITSRules
padToMaxArity rs = map (padToArity mx) rs
    where mx = maxArity rs

-- | Converts a rule list into an integer transition system
transRules :: TRSRules -> ITSRules
transRules = map leftShiftVars . padToMaxArity . map translate



rulesToITRS :: TRSRules -> ITRS
rulesToITRS rs =
    ITRS { entryPoint = entryFun,
           varList    = allVars,
           ruleList   = noFuncs ++ [entryRule] }
    where noFuncs = transRules rs
          allVars = uniqueVars noFuncs
          itsEntryPoint = findEntryPoint noFuncs
          entryFun = makeEntryPoint allVars itsEntryPoint
          entryRule = makeEntryRule entryFun itsEntryPoint

-- | Do the whole translation
endToEnd :: ITRS -> String
endToEnd (ITRS e vs rs) =
    "(GOAL COMPLEXITY)" ++ newline :
    "(STARTTERM (FUNCTIONSYMBOLS " ++ sFunName e  ++ "))" ++ newline :
    varString ++ newline :
    "(RULES" ++ newline :
    pretty rs ++  "\n)\n"
    where newline = '\n'
          varString = "(VAR " ++ pretty vs ++ ")"


-- | Make a dummy version of the entry function to the graph for KoAT
makeEntryPoint :: [Variable] -> SExp -> SExp
makeEntryPoint vars (SFun fname alist) =
    SFun (fname ++ "entry") $ take (length alist) vars
makeEntryPoint _ _ = undefined

-- | Construct a new rule for entry points so KoAT has somewhere to start
makeEntryRule :: SExp -> SExp -> ITSRule
makeEntryRule dummy @ (SFun _ da) (SFun rf _) =
    ITSRule dummy (SFun rf da) NoConstraints
makeEntryRule _ _ = undefined


-- And now we need to do something like topological sorting of the graph to
-- find the entry point.

-- | Produce all *edges* in the ITS call graph (Exp -> Exp)
getEdges :: ITSRules -> [Edge]
getEdges [] = []
getEdges (ITSRule l r _ : tl) = Edge l r : getEdges tl

-- | Produce all *nodes* in the ITS call graph
getNodes :: ITSRules -> [SExp]
getNodes [] = []
getNodes (ITSRule l r _ : tl) = l : r : getNodes tl

-- | Is the source of this edge this node?
from :: Edge -> SExp -> Bool
from (Edge a _) fn = sameHead fn a

-- | Is the destination of this edge this node?
to :: Edge -> SExp -> Bool
to (Edge _ b) fn = sameHead fn b

-- note that these things aren't at all guaranteed to be DAG,
-- so this may not provide a reasonable entry point.
-- For example, it will pick *some* element of a cycle, but I can't tell
-- you which one apriori [JTT 14-05-15]

-- | Standard topological sort, taken from wikipedia
topologicalSort :: [SExp] -> [Edge] -> [SExp]
topologicalSort [] _ = []
topologicalSort _ [] = []
topologicalSort [n] _ = [n] -- correct without, but more efficient
topologicalSort (n:tl) edges =
    stranded ++ topologicalSort tl' edges' ++ [n]
    where edges' = [ a | a <- edges, not $ from a n ]
          stranded = [ node | node <- tl, not $ any (`from` node) edges']
          tl' = [x | x <- tl, x `notElem` stranded ]

-- | Try and guess at the entry point to a ruleset
findEntryPointOld :: ITSRules -> SExp
findEntryPointOld rlist =
    head $ topologicalSort nodes edges
    where nodes = getNodes rlist
          edges = getEdges rlist

-- | Find entry points by looking for lhs of rules not mentioned in other rules
findEntryPoint :: ITSRules -> SExp
findEntryPoint rlist =
    case findEntryPointAux (getEdges rlist) rlist of
      [] -> findEntryPointOld rlist -- the ITS isn't dag, so make a guess.
      [expression] -> expression
      _ -> error "The ITS is DAG, but there are many entry points."

findEntryPointAux :: [Edge] -> ITSRules -> [SExp]
findEntryPointAux _ [] = []
findEntryPointAux edges (ITSRule l _ _:tl) =
    if any (`to` l) edges
    then findEntryPointAux edges tl
    else l : findEntryPointAux edges tl
