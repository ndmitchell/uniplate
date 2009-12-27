{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses #-}
module Uniplate.Typeable where
import Data.Generics.Uniplate.Typeable
#include "CommonInc.hs"


instance PlateAll (Map.Map [Char] Int) Int where
    plateAll = plateProject Map.toAscList Map.fromDistinctAscList


-- GENERATED

 
instance (Typeable to, Uniplate to) => PlateAll Expr to where
        plateAll (Val x1) = plate Val |+ x1
        plateAll (Var x1) = plate Var |+ x1
        plateAll (Neg x1) = plate Neg |+ x1
        plateAll (Add x1 x2) = plate Add |+ x1 |+ x2
        plateAll (Sub x1 x2) = plate Sub |+ x1 |+ x2
        plateAll (Mul x1 x2) = plate Mul |+ x1 |+ x2
        plateAll (Div x1 x2) = plate Div |+ x1 |+ x2

 
instance (Typeable to, Uniplate to) => PlateAll Stm to where
        plateAll (SDecl x1 x2) = plate SDecl |+ x1 |+ x2
        plateAll (SAss x1 x2) = plate SAss |+ x1 |+ x2
        plateAll (SBlock x1) = plate SBlock |+ x1
        plateAll (SReturn x1) = plate SReturn |+ x1

 
instance (Typeable to, Uniplate to) => PlateAll Exp to where
        plateAll (EStm x1) = plate EStm |+ x1
        plateAll (EAdd x1 x2) = plate EAdd |+ x1 |+ x2
        plateAll (EVar x1) = plate EVar |+ x1
        plateAll (EInt x1) = plate EInt |+ x1

 
instance (Typeable to, Uniplate to) => PlateAll Var to where
        plateAll (V x1) = plate V |+ x1

 
instance (Typeable to, Uniplate to) => PlateAll Typ to where
        plateAll (T_int) = plate T_int
        plateAll (T_float) = plate T_float
