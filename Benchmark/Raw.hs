
module Raw(task1, task2) where

import Data


task1 :: NExpr -> Int
task1 x = case x of
    NVal i -> if i > 0 then i else 0
    NAdd a b -> task1 a + task1 b
    NSub a b -> task1 a + task1 b
    NMul a b -> task1 a + task1 b
    NDiv a b -> task1 a + task1 b
    NNeg a -> task1 a


task2 :: NExpr -> NExpr
task2 x = case x of
    NMul a b -> NAdd (task2 a) (task2 b)
    NNeg a -> task2 a
    
    NAdd a b -> NAdd (task2 a) (task2 b)
    NSub a b -> NSub (task2 a) (task2 b)
    NDiv a b -> NDiv (task2 a) (task2 b)
    NVal x -> NVal x
