-- This module requires tricky CPP'ing
-- so that you can use 3 different Play instances

module Operations(tasksExpr) where

import Data
import DeriveCompos
import OperationsCommon
import DeriveManual

import Data.Generics.PlayEx as Play
import Data.Generics as SYB


tasksExpr = variables ++ zeros


-- * SECTION 1


variables = taskExpr "variables" [variables_raw, variables_play, variables_play2, variables_syb, variables_comp]

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

variables_play2 = alt "fold" $ playStrings $ fold concat f
    where
        f (NVar x) c = x : c
        f _ c = c

variables_syb = sybStrings $ SYB.everything (++) ([] `mkQ` f)
    where
        f (NVar x) = [x]
        f _ = []

variables_comp = compStrings f
    where
        f :: GExpr a -> [String]
        f (CVar x) = [x]
        f x = composOpFold [] (++) f x



zeros = taskExpr "zeros" [zeros_raw, zeros_play, zeros_play2, zeros_syb, zeros_comp]

zeros_raw = rawInt f
    where
        f (NDiv  x (NVal 0)) = f x + 1
        f (NVar  x    ) = 0
        f (NVal  x    ) = 0
        f (NNeg  x    ) = f x
        f (NAdd  x y  ) = f x + f y
        f (NSub  x y  ) = f x + f y
        f (NMul  x y  ) = f x + f y
        f (NDiv  x y  ) = f x + f y

zeros_play = playInt $ \x -> length [() | NDiv _ (NVal 0) <- Play.everything x]

zeros_play2 = alt "fold" $ playInt $ fold sum f
    where
        f (NVar x) c = 1 + c
        f _ c = c

zeros_syb = sybInt $ SYB.everything (+) (0 `mkQ` f)
    where
        f (NDiv _ (NVal 0)) = 1
        f _ = 0

zeros_comp = compInt f
    where
        f :: GExpr a -> Int
        f (CDiv x (CVal 0)) = 1 + f x 
        f x = composOpFold 0 (+) f x



simplify = taskExpr "simplify" [simplify_raw,simplify_play,simplify_play2,simplify_syb,simplify_compos]

simplify_raw = rawExpr f
    where
        f (NSub x y) = NAdd (f x) (NNeg (f y))
        f (NAdd x y) = if x1 == y1 then NMul (NVal 2) x1 else NAdd x1 y1
            where (x1,y1) = (f x,f y)
        f (NMul x y) = NMul (f x) (f y)
        f (NDiv x y) = NDiv (f x) (f y)
        f (NNeg x) = NNeg (f x)
        f x = x

simp (NSub x y)           = NAdd x (NNeg y)
simp (NAdd x y) | x == y  = NMul (NVal 2) x
simp x                    = x

simplify_play = playExpr $ traverse simp

simplify_play2 = alt "rewrite" $ playExpr $ rewrite f
    where
        f (NSub x y)           = Just $ NAdd x (NNeg y)
        f (NAdd x y) | x == y  = Just $ NMul (NVal 2) x
        f x                    = Nothing

simplify_syb = sybExpr $ everywhere (mkT simp)

simplify_compos = compExpr f
    where
        f :: GExpr a -> GExpr a
        f (CSub x y) = CAdd (f x) (CNeg (f y))
        f (CAdd x y) = if x1 == y1 then CMul (CVal 2) x1 else CAdd x1 y1
            where (x1,y1) = (f x,f y)
        f x = composOp f x
