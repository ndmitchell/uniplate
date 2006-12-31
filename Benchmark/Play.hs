
module Play(task1_over, task1_fold, task2_under, task2_over, task2_compos) where

import Data.Play
import Data
import Data.List(foldl')


task1_over :: NExpr -> Int
task1_over x = sum [i | NVal i <- allOver x, i > 0]


task1_fold :: NExpr -> Int
task1_fold = fold (foldl' (+) 0) $ \x a -> case x of
    NVal i | i > 0 -> i
    _ -> a


instance Play NExpr where
    replaceChildren x =
        case x of
            NAdd x y -> playTwo NAdd x y
            NSub x y -> playTwo NSub x y
            NDiv x y -> playTwo NDiv x y
            NMul x y -> playTwo NMul x y
            NNeg x   -> playOne NNeg x
            x -> playDefault x



task2_action (NNeg x) = task2_action x
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

