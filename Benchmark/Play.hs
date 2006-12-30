
module Play(task1) where

import Data.Play
import Data


task1 :: NExpr -> Int
task1 x = sum [i | NVal i <- allOver x, i > 0]


instance Play NExpr where
    replaceChildren x =
        case x of
            NAdd x y -> playTwo NAdd x y
            NSub x y -> playTwo NSub x y
            NDiv x y -> playTwo NDiv x y
            NMul x y -> playTwo NMul x y
            NNeg x   -> playOne NNeg x
            x -> playDefault x
