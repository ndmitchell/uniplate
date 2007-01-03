{-# OPTIONS_GHC -fglasgow-exts #-}

module SYB(task1, task2) where

import Data
import Data.Generics


task1 :: NExpr -> Int
task1 = everything (+) (0 `mkQ` f)
    where
        f (NVal i) | i > 0 = i
        f x = 0


task2 :: NExpr -> NExpr
task2 x = everywhere (mkT f) x
    where
        f (NMul a b) = NAdd (f a) (f b)
        f (NNeg a) = a
        f x = x
