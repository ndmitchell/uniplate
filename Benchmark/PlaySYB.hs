
module PlaySYB(task1_everything, task1_fold, task2_traverse, task2_descend) where

import Data.Generics.PlaySYB
import Data
import Data.List(foldl')


task1_everything :: NExpr -> Int
task1_everything x = sum [i | NVal i <- everything x, i > 0]


task1_fold :: NExpr -> Int
task1_fold = fold (foldl' (+) 0) $ \x a -> case x of
    NVal i | i > 0 -> i
    _ -> a


task2_traverse :: NExpr -> NExpr
task2_traverse = traverse f
    where
        f (NNeg x) = f x
        f (NMul x y) = NAdd x y
        f x = x


task2_descend :: NExpr -> NExpr
task2_descend x = case x of
    NMul a b -> NAdd (task2_descend a) (task2_descend b)
    NNeg a -> task2_descend a
    _ -> descend task2_descend x
