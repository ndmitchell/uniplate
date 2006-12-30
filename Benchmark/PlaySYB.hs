
module PlaySYB(task1, task2_under, task2_over, task2_compos) where

import Data.PlaySYB
import Data


task1 :: NExpr -> Int
task1 x = sum [i | NVal i <- allOver x, i > 0]


task2_action (NNeg x) = x
task2_action (NMul x y) = NAdd x y
task2_action x = x


task2_under :: NExpr -> NExpr
task2_under = mapUnder task2_action

task2_over :: NExpr -> NExpr
task2_over = mapOver task2_action

task2_compos :: NExpr -> NExpr
task2_compos x = case x of
    NMul a b -> NAdd (task2_compos a) (task2_compos b)
    NNeg a -> task2_compos a
    _ -> compos task2_compos x
