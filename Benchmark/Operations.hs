-- This module requires tricky CPP'ing
-- so that you can use 3 different Play instances

module Operations(tasksExpr) where

import Data
import DeriveCompos
import OperationsCommon
import DeriveManual

import Data.Generics.PlayEx as Play
import Data.Generics as SYB


tasksExpr = variables


-- * SECTION 1


variables = taskExpr "variables" [variables_raw, variables_play, variables_syb, variables_comp]


variables_raw = rawStrings f
    where
        f (NVar  x    ) = [x]
        f (NVal  x    ) = []
        f (NNeg  x    ) = f x
        f (NAdd  x y  ) = f x ++ f y
        f (NSub  x y  ) = f x ++ f y
        f (NMul  x y  ) = f x ++ f y
        f (NDiv  x y  ) = f x ++ f y


variables_play = playStrings $ \x -> [y | NVar y <- Play.everything x]

variables_syb = sybStrings $ SYB.everything (++) ([] `mkQ` f)
    where
        f (NVar x) = [x]
        f _ = []

variables_comp = compStrings f
    where
        f :: GExpr a -> [String]
        f (CVar x) = [x]
        f x = composOpFold [] (++) f x
