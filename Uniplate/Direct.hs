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
