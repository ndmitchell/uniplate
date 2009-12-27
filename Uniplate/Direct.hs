{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses #-}
module Uniplate.Direct where
import Data.Generics.Uniplate.Direct
#include "CommonInc.hs"

instance Biplate (Map.Map [Char] Int) Int where
    biplate = plateProject Map.toAscList Map.fromDistinctAscList

instance Biplate [Map.Map [Char] Int] Int where
    biplate (x:xs) = plate (:) |+ x ||+ xs
    biplate x = plate x

-- GENERATED

 
instance Uniplate Expr where
        uniplate (Neg x1) = plate Neg |* x1
        uniplate (Add x1 x2) = plate Add |* x1 |* x2
        uniplate (Sub x1 x2) = plate Sub |* x1 |* x2
        uniplate (Mul x1 x2) = plate Mul |* x1 |* x2
        uniplate (Div x1 x2) = plate Div |* x1 |* x2
        uniplate x = plate x

 
instance Uniplate Exp where
        uniplate (EStm x1) = plate EStm |+ x1
        uniplate (EAdd x1 x2) = plate EAdd |* x1 |* x2
        uniplate x = plate x

 
instance Uniplate Stm where
        uniplate (SAss x1 x2) = plate (SAss x1) |+ x2
        uniplate (SBlock x1) = plate SBlock ||* x1
        uniplate (SReturn x1) = plate SReturn |+ x1
        uniplate x = plate x

 
instance Biplate Stm Exp where
        biplate (SAss x1 x2) = plate (SAss x1) |* x2
        biplate (SBlock x1) = plate SBlock ||+ x1
        biplate (SReturn x1) = plate SReturn |* x1
        biplate x = plate x

 
instance Biplate Exp Stm where
        biplate (EStm x1) = plate EStm |* x1
        biplate (EAdd x1 x2) = plate EAdd |+ x1 |+ x2
        biplate x = plate x

 
instance Biplate Exp [Stm] where
        biplate (EStm x1) = plate EStm |+ x1
        biplate (EAdd x1 x2) = plate EAdd |+ x1 |+ x2
        biplate x = plate x

 
instance Biplate Stm [Stm] where
        biplate (SAss x1 x2) = plate (SAss x1) |+ x2
        biplate (SBlock x1) = plate SBlock |* x1
        biplate (SReturn x1) = plate SReturn |+ x1
        biplate x = plate x

 
instance Biplate Stm Stm where
        biplate = plateSelf

 
instance Uniplate [Stm] where
        uniplate ((:) x1 x2) = plate (:) |+ x1 |* x2
        uniplate x = plate x

 
instance Biplate Stm Var where
        biplate (SDecl x1 x2) = plate (SDecl x1) |* x2
        biplate (SAss x1 x2) = plate SAss |* x1 |+ x2
        biplate (SBlock x1) = plate SBlock ||+ x1
        biplate (SReturn x1) = plate SReturn |+ x1

 
instance Uniplate Var where
        uniplate x = plate x

 
instance Biplate Exp Var where
        biplate (EStm x1) = plate EStm |+ x1
        biplate (EAdd x1 x2) = plate EAdd |+ x1 |+ x2
        biplate (EVar x1) = plate EVar |* x1
        biplate x = plate x

 
instance Biplate Company Salary where
        biplate (C x1) = plate C ||+ x1

 
instance Biplate Company Dept where
        biplate (C x1) = plate C ||* x1

 
instance Biplate Dept Salary where
        biplate (D x1 x2 x3) = plate (D x1) |+ x2 ||+ x3

 
instance Uniplate Salary where
        uniplate x = plate x

 
instance Uniplate Dept where
        uniplate (D x1 x2 x3) = plate (D x1 x2) ||+ x3

 
instance Biplate Employee Salary where
        biplate (E x1 x2) = plate (E x1) |* x2

 
instance Biplate Unt Salary where
        biplate (PU x1) = plate PU |+ x1
        biplate (DU x1) = plate DU |+ x1

 
instance Biplate Unt Dept where
        biplate (DU x1) = plate DU |* x1
        biplate x = plate x

 
instance Biplate (Either String Int) Int where
        biplate (Right x1) = plate Right |* x1
        biplate x = plate x

 
instance Biplate (Either String Int) Char where
        biplate (Left x1) = plate Left ||* x1
        biplate x = plate x

 
instance Biplate [([Char], Int)] Int where
        biplate ((:) x1 x2) = plate (:) |+ x1 ||+ x2
        biplate x = plate x

 
instance Biplate ([Char], Int) Int where
        biplate (x1, x2) = plate ((,) x1) |* x2
