module PolyRing
( javaPath
, wholeClass )where
import Graph

makeFunctions :: Int -> [Exp]
makeFunctions degree = [ makeFunction degree i | i <- [1 .. degree ]]

modify :: Exp -> Exp
modify (NumExpr ne) = NumExpr (Plus ne (Int $ -1))
modify other = undefined

makeDecrArgs :: Int -> [Exp] -> [Exp]
makeDecrArgs ind [] = []
makeDecrArgs ind rest @ (hd : tl)
 | ind == 0 = modify hd : tl
 | otherwise = hd : makeDecrArgs (ind - 1) tl

makeEdges :: [Exp] -> Int -> Int -> [Edge]
makeEdges funcs origInd destInd
 | origInd < destInd =
     let passThrough = Edge (Node origin) (Node destination)
                       (argList origin) NoConstraints
         decr = Edge (Node origin) (Node destination) decrement decGuard
         origin = funcs !! origInd
         destination = funcs !! destInd
         decrement = makeDecrArgs origInd (argList origin)
         decGuard = Constraints $ NumConstr $
                    LocalGTE (Int 0) $ Var (nextVariable $ destInd + 1) in
     [decr, passThrough]

 | otherwise =
     let origin = funcs !! origInd
         destination = funcs !! destInd
         decrement = makeDecrArgs origInd (argList origin)
         decGuard = Constraints $ NumConstr $
                    LocalGT (Var (nextVariable $ destInd + 1)) $ (Int 0) in
     [Edge (Node origin) (Node destination) decrement decGuard,
      Edge (Node origin) (Node (NumExpr (Int 0))) (argList origin) NoConstraints]


allEdges :: [Exp] -> [[Edge]]
allEdges funcs =
    map (\ (o,d) -> makeEdges funcs o d) tups
    where tups = [(a,a+1) | a <- [0..length funcs - 2]] ++ [(length funcs - 1, 0)]

makeGraphComp :: Exp -> [Edge] -> (Node, [Edge])
makeGraphComp fn edges = (Node fn, edges)

makeGraph :: Int -> Graph
makeGraph size =
    zipWith (\ func edges -> makeGraphComp func edges) functions edgeSets
    where functions = makeFunctions size
          edgeSets = allEdges functions

staticHead sz = "public class PolyRing" ++ show sz ++ "{\n"
staticMain sz = "\n\n\tpublic static void main(String[] argv){\n" ++
                '\t' : '\t' : "int start = argv.length;\n" ++
                '\t' : '\t' : cname ++ " me = new " ++ cname ++"();\n" ++
                "\t\tme." ++ toJava False 0 (Function "function1" $
                               map (NumExpr. Var . Variable) $ replicate sz "start") ++ ";\n"
                ++ "\t}\n}"
                where cname = "PolyRing" ++ show sz

wholeClass :: Bool -> Int -> String
wholeClass hr sz =
    staticHead sz ++ toJava hr 0 (makeGraph sz) ++ staticMain sz

javaPath :: Bool -> Int -> String
javaPath hr sz = "PolyRing" ++ show sz ++ ".java"
