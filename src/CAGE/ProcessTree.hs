-- Maybe we should represent dependencies as a tree?
-- Jordan Thayer  2015-05-14T10:35:15-04:00
{-# LANGUAGE DoAndIfThenElse #-}
module ProcessTree
( Process(..)
, Status(..)
, Delta(..)
, prettyPrint
, runTree
, nSteps
) where

import ShellProcess
import Control.Monad
import Control.Exception.Base

data Status = PreStart | InProgress | Completed deriving (Eq, Show)

data Delta = SP ShellProcess | Mutation (Process -> IO Process) | NoOP

data Process =
    Process { name :: String
            , status    :: Status
            , invocation :: Delta
            , subProcesses :: [Process]
            }
  | Failure { name :: String }

{-|
  Two Processes are considered Eq when either they are both failing and have
  the same name, or they are both processes with either no invocation, but the
  same name, or equivalent invocations.
 -}
instance Eq Process where
    Failure nm1 == Failure nm2 = nm1 == nm2
    Failure _ == _ = False
    _ == Failure _ = False
    p1 @ (Process {}) == p2 @ (Process {}) =
        case (invocation p1, invocation p2) of
          (NoOP, NoOP) -> name p1 == name p2
          (Mutation _ , Mutation _) -> name p1 == name p2
          (_ , NoOP) -> False
          (NoOP , _) -> False
          (Mutation _, SP _) -> False
          (SP _, Mutation _) -> False
          (SP s1, SP s2) -> shellString s1 == shellString s2

instance Show Process where
    show (Failure nm) = "FAILED: " ++ show nm
    show p @ (Process {}) =
        name p ++ ' ' : show (status p)

data Dictionary
    = NoChange
    | Replacement (Process, Process)
    | Function (Process, Process -> IO Process)
    | Cons Dictionary Dictionary

instance Show Dictionary where
    show (Cons d1 d2) = show d1 ++ ' ' : show d2
    show NoChange = "NoChange"
    show (Replacement (p1,p2)) = show p1 ++ " -> " ++ show p2
    show (Function (p1, _)) = "Function on " ++ show p1

-- | Give me a fixed number of tabs before the string of a process
indent :: Int -> Process -> String
indent i p =
    if i == 0
    then show p
    else '\t' : indent (i - 1) p

-- | Draw the tree in some nice way
pretty :: Int -> Process -> String
pretty i f @ (Failure _) = indent i f
pretty i p =
    me ++ children
    where me = indent i p
          children = concatMap (\ sp -> '\n' : pretty (i + 1) sp)
                     $ subProcesses p

-- | pretty print, starting with the root.
prettyPrint :: Process -> String
prettyPrint = pretty 0

-- We can model process groups as a dummy parent relying on each member
-- of the group, but with no invocation itself.

-- | is this a failing node?
isFailure :: Process -> Bool
isFailure (Failure _) = True
isFailure _ = False

-- | Is the given Process supported (are subtasks done)
supported :: Process -> Bool
supported (Failure {}) = False
supported p = all (\ sp -> not (isFailure sp) && status sp == Completed) $
              subProcesses p

-- | Is the given Process ready to be run
readyToRun :: Process -> Bool
readyToRun (Failure _) = False
readyToRun p = status p == PreStart && supported p

-- | Moves a process to the InProgress or Completed state
start :: Process -> Dictionary
start (Failure _) = NoChange
start p
 | any isFailure $ subProcesses p = Replacement (p,Failure (name p))
 | readyToRun p = invoke p
 | otherwise = NoChange

-- | Moves a process to the Completed State
reapInProgress :: Process -> Dictionary
reapInProgress (Failure _) = NoChange
reapInProgress p
    | status p == InProgress  && completed p =
        Replacement (p, p { status = Completed })
    | otherwise = NoChange

-- | Start the process that the Process describes
invoke :: Process -> Dictionary
--invoke = invokeDryRun
invoke = invokeBlocking

{-|
  Process to run -> Change to Process tree

  Uses / Assumes blocking shell calls.  No parallelism when using this.
 -}
invokeBlocking :: Process -> Dictionary
invokeBlocking (Failure _) = NoChange
invokeBlocking p @ (Process _ _ (Mutation mutate) _) =
    Function (p, mutate)
invokeBlocking p @ (Process _ _ NoOP _) =
    Replacement (p, p { status = Completed })
invokeBlocking p @ (Process _ _ (SP s) _) =
    Function (p, makeShellMutation)
    where makeShellMutation _ = do
          failure <- invokeSP s
          if not failure
          then return $ p { status = Completed }
          else return $ Failure (name p)

{- |
   Process To Run -> Change to Process Tree

   Don't actually run any shell commands, just print what you would have done,
   and assume you completed successfully.  For haskell processes
   (e.g. tree extending), we still run as normal.
 -}
invokeDryRun :: Process -> Dictionary
invokeDryRun (Failure _) = NoChange
invokeDryRun p @ (Process _ _ (Mutation mutate) _) = Function (p, mutate)
invokeDryRun p @ (Process _ _ NoOP _) =
    Replacement (p, p { status = Completed })
invokeDryRun p @ (Process _ _ (SP _) _) =
    Replacement (p, p { status = InProgress })

-- | Determine if a process was in progress
inProgress :: Process -> Bool
inProgress (Failure _) = False
 -- I really want to check the fs here [JTT 14-05-15]
inProgress p = status p == InProgress

-- | Determine if a process was completed
completed :: Process -> Bool
completed (Failure _) = True
 -- I really want to check the fs here [JTT 14-05-15]
completed _ = True

-- | Is Process mention in dictionary?
mentioned :: Process -> Dictionary -> Bool
mentioned p (Cons d1 d2) = mentioned p d1 || mentioned p d2
mentioned (Failure _) _ = False -- there aren't any functions on failure, right?
mentioned _ NoChange = False
mentioned p (Replacement (src, _)) = p == src
mentioned p (Function (src, _)) = p == src

-- Step on subprocesses. mutually recursive helper of step
stepSP :: [Process] -> Dictionary -> Dictionary
stepSP [] accum = accum
stepSP (hd:tl) accum =
    case delta of
      NoChange -> stepSP tl accum
      _ -> stepSP tl $ Cons delta accum
    where delta = step hd accum

-- | Moves the process tree forward one step
step :: Process -> Dictionary -> Dictionary
step (Failure _) accum = accum
step p accum
 | mentioned p accum = accum
 | status p == Completed = accum
 | readyToRun p = Cons accum $ start p
 | inProgress p = Cons accum $ reapInProgress p
 | otherwise =  stepSP (subProcesses p) accum

-- | run the process forward n steps.  Mostly a debugging tool.
nSteps :: Int -> Process -> IO Process
nSteps _ f @ (Failure _) = return f
nSteps ind p =
    if ind == 0 then evaluate p
    else case delta of
           NoChange -> evaluate p
           _ -> nSteps (ind - 1) =<< apply delta p
        where delta = step p NoChange

-- | Run the process represented by a process tree end to end
--  Either you'll get back Failure because some subprocess failed,
--  Or you'll get back the completed process tree
runTree :: Process -> IO Process
runTree f @ (Failure _) = return f
runTree p
  | status p == Completed = return p
  | otherwise =
    case delta of
      NoChange -> evaluate p
      _ -> runTree =<< apply delta p
    where delta = step p NoChange

applyRecur :: String -> Status -> Delta -> [IO Process] -> IO Process
applyRecur nm stat invoc childs =
    liftM (Process nm stat invoc) (sequence childs)

-- | Apply the changes in a dictionary to a process tree
-- assumes this only appears once in dict
-- assumes that if a parent is changed, its children aren't.
apply :: Dictionary -> Process -> IO Process
apply dict this
    | not $ mentioned this dict =
        applyRecur (name this) (status this) (invocation this)
      $ map (apply dict) (subProcesses this)
    | otherwise =
        case dict of
          Cons hd tl ->
              if mentioned this hd
              then apply hd this
              else apply tl this
          NoChange -> return this
          -- you need the checks because of mentioned on Cons lists.
          Replacement(orig, update) ->
              if this == orig
              then return update
              else return this
          Function(orig, mutation) ->
              if this == orig
              then mutation this
              else return this
