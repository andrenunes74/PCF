module TPC2 where

{------------------------------------------------
    3 variables to test our languange (memory)
-------------------------------------------------}
data Vars = X 
          | Y 
          | Z 
        deriving (Show, Eq)

-- Function that prints the memory of a program
printMemory :: (Vars -> Double) -> IO ()
printMemory mem = do
  putStrLn "X: " >> print (mem X)
  putStrLn "Y: " >> print (mem Y)
  putStrLn "Z: " >> print (mem Z)

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
    (Assignment;Sequence of operations;If then else;While loops;Message logs)
------------------------------------------------------------------------------}
data WhP = Asg Vars LTerm 
         | Seq WhP WhP 
         | Ife BTerm WhP WhP 
         | Wh BTerm WhP 
         | Wr String WhP
        deriving Show

-- Memory management
chMem :: Vars -> Double -> (Vars -> Double) -> (Vars -> Double)
chMem v r m = \u -> if u == v then r else m u

-- Semantics for our While programs
wsem :: WhP -> (Vars -> Double) -> IO (Vars -> Double)
wsem (Asg v t) m = do
  let updatedMemory = chMem v (sem t m) m
  return updatedMemory
wsem (Seq p q) m = do
  m' <- wsem p m
  m'' <- wsem q m'
  return m''
wsem (Ife b p q) m = do
  if (bsem b m)
    then wsem p m
    else wsem q m
wsem (Wh b p) m = do
  if (bsem b m)
    then do
      m' <- wsem p m
      wsem (Wh b p) m'
    else return m
wsem (Wr s p) m = do
  putStr s
  wsem p m


{---------------------------------------------------------------------------------------------------------------------
***************************************************** Tests **********************************************************
----------------------------------------------------------------------------------------------------------------------}
exampleProgram1 = Seq (Asg X (Leaf (Right 5.0)))(Wr ("Message Log1"++"Message Log2") ((Asg X (Add (Leaf (Left X)) (Leaf (Right 10.0))))))
exampleProgram2 = Seq (Asg X (Leaf (Right 5.0)))(Wr "Message Log1" ((Wr "Message Log2" ((Asg X (Add (Leaf (Left X)) (Leaf (Right 10.0))))))))

-- Simple main function that initializes memory with the value 0 and executes for each exampleProgram defined above
main :: IO ()
main = do
  let initialMemory1 = \_ -> 0.0 
  --printMemory initialMemory
  updatedMemory1 <- wsem exampleProgram1 initialMemory1
  let finalValueOfX1 = updatedMemory1 X
  putStrLn ("\n1 -> Final value of X: " ++ show finalValueOfX1)
  let initialMemory2 = \_ -> 0.0 
  --printMemory initialMemory
  updatedMemory2 <- wsem exampleProgram2 initialMemory2
  let finalValueOfX2 = updatedMemory2 X
  putStrLn ("\n2 -> Final value of X: " ++ show finalValueOfX2)
