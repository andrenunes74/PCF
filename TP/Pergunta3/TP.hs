module TP where

import Probability
import Control.Monad (replicateM)

{------------------------------------------------
    3 variables to test our language (memory)
-------------------------------------------------}
data Vars = X 
          | Y 
          | Z 
        deriving (Show, Eq)

{-----------------------------------------------------------
    Linear terms of our language and arithmetic operations 
    (sum and multiplication)
------------------------------------------------------------}
data LTerm = Leaf (Either Vars Double) 
           | Mult Double LTerm  
           | Add LTerm LTerm
        deriving Show

-- Semantics for our linear terms
sem :: LTerm -> (Vars -> Double) -> Double
sem (Leaf (Left v)) m = m v
sem (Leaf (Right r)) m = r
sem (Mult s t) m = let r = sem t m in s * r
sem (Add t1 t2) m = let r1 = sem t1 m
                        r2 = sem t2 m in r1 + r2

{-----------------------------------------
    Boolean terms 
    (logic operations for our language)
------------------------------------------}
data BTerm = Leq LTerm LTerm
           | Conj BTerm BTerm 
           | Neg BTerm 
        deriving Show

-- Semantics for our boolean terms
bsem :: BTerm -> (Vars -> Double) -> Bool
bsem (Leq t1 t2) m = let r1 = sem t1 m
                         r2 = sem t2 m
                     in  if r1 <= r2 then True else False
bsem (Conj b1 b2) m = let v1 = bsem b1 m
                          v2 = bsem b2 m
                      in v1 && v2
bsem (Neg b) m = let v = bsem b m in not v

{-----------------------------------------------------------------------------
    While programs and their operations 
    (Assignment;Sequence of operations;If then else;While loops;ProbChoice)
------------------------------------------------------------------------------}
data WhP = Asg Vars LTerm 
         | Seq WhP WhP 
         | Ife BTerm WhP WhP 
         | Wh BTerm WhP 
         | ProbChoice ProbRep WhP WhP  -- New probabilistic choice program
        deriving Show

-- Memory management
chMem :: Vars -> Double -> (Vars -> Double) -> (Vars -> Double)
chMem v r m = \u -> if u == v then r else m u

wsem :: WhP -> (Vars -> Double) -> [Vars -> Double] -> Dist [Vars -> Double]
wsem (Asg v t) m states = return (chMem v (sem t m) m : states)
wsem (Seq p q) m states = do
  states' <- wsem p m states
  wsem q m states'
wsem (Ife b p q) m states = do
  if bsem b m
    then wsem p m states
    else wsem q m states
wsem (Wh b p) m states = do
  if bsem b m
    then do
      states' <- wsem p m states
      wsem (Wh b p) m (states' ++ states)
    else return (states ++ [m])
wsem (ProbChoice p p1 p2) m states = do
  choice <- choose p p1 p2
  wsem choice m states


allMemoryStates :: WhP -> Dist [Vars -> Double]
allMemoryStates program = wsem program (\_ -> 0) []

