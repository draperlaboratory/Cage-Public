{- |
  Author: Cody Roux
  Company: Draper Laboratories
  
  Module: Dep

  Simple dependency analysis: for each defined function in an ITS,
  return the argument positions on which the function complexity *may*
  depend.

-}

module Dep where

import AST
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Control.Monad

data Position = Position Int
                deriving (Eq, Ord, Show)

-- The idea is similar to size change termination: for each transition,
-- build a "delta-graph" which captures which arguments depend on which and whether 
-- they have changed. Then, the transitive closue of the graph compositions is
-- computed, and each cycle is examined. If there is a "horizontal" delta, then
-- the function complexity may depend on that argument.

-- | An element of 'DepType' captures the "depends on" relation, e.g.
-- `DeltaDep (Position 3)` means the current element depends on the 3rd argument
-- of the caller. `DeltaDep` captures unknown (but non-trivial) dependency,
-- and `EqDep` captures exact equality.
data DepType = DeltaDep Position | EqDep Position
             deriving (Show, Eq)

-- | Unbox the position in a DepType
getPos :: DepType -> Position
getPos (DeltaDep p) = p
getPos (EqDep p)    = p

-- | An element of 'Deps' is a map from the positions of a function call
-- to the list of dependencies of that location on the locations of the
-- caller function in the lhs.
data Deps = Deps { depsMap :: Map.Map Position [DepType] }
          deriving (Show, Eq)

-- Compose dependencies from left to right
-- | The composition of two dependencies, when considered
-- from left-to-right (usual function call order).
compDep :: DepType -> DepType -> DepType
compDep (DeltaDep p) _         = DeltaDep p
compDep (EqDep p) (DeltaDep _) = DeltaDep p
compDep (EqDep p) (EqDep _)    = EqDep p

-- | A 'DGraph' is the pair of caller and callee ids (to know if the graphs can be
-- composed) along with the dependencies between the callee argument positions and
-- the caller positions
data DGraph = DGraph { callerId :: Ident, calleeId :: Ident, edges :: Deps }
            deriving Show

-- | Return the set of 'DGraph's corresponding to the calls in an 'ITRS'.
mkDGraphs :: ITRS -> [DGraph]
mkDGraphs (ITRS _ _ rs) = catMaybes $ map mkDGraph rs

-- | Return the 'DGraph' corresponding to the call in an 'ITSRule', if there is one.
mkDGraph :: ITSRule -> Maybe DGraph
mkDGraph (ITSRule l r c) =
    case (l, r) of
      (SFun f fArgs, SFun g gArgs) ->
          Just $ DGraph f g $ mkEdges fArgs gArgs c
      _ -> Nothing

-- | Given a set of 'Variable's in the correct order, create the
-- map from the variable name to the position in the list.
mkMap :: [Variable] -> Map.Map Variable Position
mkMap vs = mkMapAux 0 vs
    where mkMapAux _ [] = Map.empty
          mkMapAux n (x:xs) = Map.insert x (Position n) (mkMapAux (n+1) xs)


-- | Given a list of 'Variable's `[x1,..., xn]`, create the constraints
-- `x1 == x1 & x2 == x2 & ... & xn == xn`.
mkEqConstr :: [Variable] -> Constraints
mkEqConstr []     = NoConstraints
mkEqConstr ctrs = Constraints $ mkEqBoolExp ctrs
    where mkEqBoolExp [x] = NumConstr $ LocalEq (Var x) (Var x)
          mkEqBoolExp (x:xs) =  And (LocalEq (Var x) (Var x)) $ mkEqBoolExp xs
          mkEqBoolExp _ = undefined


-- | Given a set of lhs argument variables, a set of rhs variables, and a set of
-- constraints, generate the dependency graph for the call lhs -> rhs
mkEdges :: [Variable] -> [Variable] -> Constraints -> Deps
mkEdges fArgs gArgs cstrs = let allCstrs = concatConstr (mkEqConstr gArgs) cstrs
                                fArgsMap = mkMap fArgs
                            in
                              Deps $ mkEdgesAux 0 gArgs fArgsMap allCstrs


mkEdgesAux :: Int -> [Variable] -> 
              Map.Map Variable Position -> Constraints -> Map.Map Position [DepType]
mkEdgesAux _ [] _ _ = Map.empty
mkEdgesAux n (y:ys) fArgs cs = Map.insert (Position n) (mkEdge y fArgs cs) $
                               mkEdgesAux (n+1) ys fArgs cs

-- | Given a variable, a map that associates variables to their position in the caller,
-- and a set of constraints, return the dependencies of the current variable on the variables
-- in the caller
mkEdge :: Variable -> Map.Map Variable Position -> Constraints -> [DepType]
mkEdge v fArgs cstr =
    concat $ map (mkDepConstr v fArgs) $ constrToList cstr

-- | Given a variable, a position map and a single 'NumConstr' constraint, return the dependencies for that
-- constraint
mkDepConstr :: Variable -> Map.Map Variable Position -> NumConstr -> [DepType]
mkDepConstr v m (LocalEq (Var x) (Var y)) | v == x = maybeToList $ liftM EqDep $ Map.lookup y m
                                          | v == y = maybeToList $ liftM EqDep $ Map.lookup x m
                                          | otherwise = []
mkDepConstr v m c = let vs = uniqueVars c in
                    if v `elem` vs
                    then catMaybes $ map (liftM DeltaDep . (flip Map.lookup m)) vs
                    else []


-- | Return the set of conjuncts in a constraint
constrToList :: Constraints -> [NumConstr]
constrToList NoConstraints = []
constrToList (Constraints c) = boolExpToList c
    where boolExpToList (NumConstr n) = [n]
          boolExpToList (And n ns) = n : boolExpToList ns



-- | Given two 'DGraph's, return the composite of the graphs if it exists
composeGraph :: DGraph -> DGraph -> Maybe DGraph
composeGraph (DGraph f g eg1) (DGraph g' h eg2) | g == g' = Just $ DGraph f h $ composeEdges eg1 eg2
                                                 | otherwise = Nothing

composeEdges :: Deps -> Deps -> Deps
composeEdges (Deps m1) (Deps m2) = Deps $ composeEdgesAux
    where composeEdgesAux = Map.map (concat . map (getDeps m1)) m2
          getDeps m dt = case Map.lookup (getPos dt) m of
                           Nothing -> []
                           -- Careful of the composition order here!
                           Just deps -> map (flip compDep dt) deps

-- TODO: optimize this function
-- | Computes the transitive closue (under composition) of a set of transition graphs
mkClosure :: [DGraph] -> [DGraph]
mkClosure gList = let newList = mkClosureStep gList in
                  if null newList 
                  then gList
                  else mkClosure (newList ++ gList)
    where mkClosureStep gs = foldl (\new g -> compSet g gs ++ new) [] gs


eqGraph :: DGraph -> DGraph -> Bool
eqGraph (DGraph f g eg) (DGraph f' g' eg') =
    if f == f' && g == g' then
        -- using the derived equality for maps
        eg == eg'
    else
        False

-- | `compSet g gs` Iterates (pre) composing g over all the gs, and returns the set of
-- /new/ resulting graphs
compSet :: DGraph -> [DGraph] -> [DGraph]
compSet g gs = filter (\h -> all (not . eqGraph h) gs) $ catMaybes $ map (composeGraph g) gs


argDep :: Position -> DepType -> Bool
argDep p (DeltaDep q) = p == q
argDep _ _            = False

argDepList :: Position -> [DepType] -> Maybe Position
argDepList p l = if any (argDep p) l then Just p else Nothing

-- | The type of argument dependencies: for a given function, a list of positions
-- on which the function depends in asymptotic complexity
data ArgDep = ArgDep { depFun :: Ident, depPos :: [Position] }
              deriving Show


-- | Given a 'DGraph', return the 'ArgDep' that corresponds to the arguments on which it
-- may recursively depend.
recDep :: DGraph -> Maybe ArgDep
recDep (DGraph f f' (Deps ms)) | f == f' =
                                   let deps = catMaybes $ map (uncurry argDepList) $ Map.assocs ms
                                   in
                                     if null deps then Nothing
                                     else Just $ ArgDep f $ deps
                                         
recDep _ = Nothing

recDeps :: [DGraph] -> [ArgDep]
recDeps = compactDeps . catMaybes . map recDep

-- | Collects the dependencies for a single function
mergeDeps :: ArgDep -> [ArgDep] -> ArgDep
mergeDeps d [] = d
mergeDeps (ArgDep f ps) ((ArgDep f' ps'):ds) | f == f'   = mergeDeps (ArgDep f (nub $ ps ++ ps')) ds
                                             | otherwise = mergeDeps (ArgDep f ps) ds

-- | Takes a list of 'ArgDep's and gathers all the dependencies for a given function,
-- for each function that appears in the list.
compactDeps :: [ArgDep] -> [ArgDep]
compactDeps [] = []
compactDeps (d:ds) = mergeDeps d ds : compactDeps (filter (\d' -> depFun d /= depFun d') ds)

interDep :: [Position] -> [Position] -> Bool
interDep ps ds = not $ null $ intersect ps ds

-- | Compose an 'ArgDep' with a 'DGraph'
compDepGraph :: ArgDep -> DGraph -> Maybe ArgDep
compDepGraph (ArgDep f ps) (DGraph f' g (Deps eg))
    | f == f' =
        Just $ ArgDep g $ map fst $ filter (\(_, deps) -> interDep ps (map getPos deps))
                 $ Map.assocs eg
    | otherwise = Nothing


allDeps :: ArgDep -> [DGraph] -> [ArgDep]
allDeps a = compactDeps . catMaybes . map (compDepGraph a)

interArgDep :: ArgDep -> ArgDep -> Bool
interArgDep (ArgDep f p) (ArgDep f' p') = f == f' && interDep p p'

interArgDeps :: [ArgDep] -> [ArgDep] -> Bool
interArgDeps ds ds' = or [interArgDep d d' | d <- ds, d' <- ds']


funLeak :: Ident -> [Position] -> [DGraph] -> Bool
funLeak f pos gs = interArgDeps taint rDeps
    where taint = allDeps (ArgDep f pos) gs
          rDeps = recDeps gs


leaks :: ITRS -> [Position] -> [DGraph] -> Bool
leaks (ITRS (SFun f _) _ _) pos deps = funLeak f pos deps
leaks _ _ _ = error "leaks: ill formed start term!"
