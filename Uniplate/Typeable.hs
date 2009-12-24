{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses #-}
module Uniplate.Typeable where
import Data.Generics.Uniplate.Typeable
#include "CommonInc.hs"

 
instance Uniplate Expr where
        uniplate = uniplateAll
 
instance (Typeable to, Uniplate to) => PlateAll Expr to where
        plateAll (Val x1) = plate Val |+ x1
        plateAll (Var x1) = plate Var |+ x1
        plateAll (Neg x1) = plate Neg |+ x1
        plateAll (Add x1 x2) = plate Add |+ x1 |+ x2
        plateAll (Sub x1 x2) = plate Sub |+ x1 |+ x2
        plateAll (Mul x1 x2) = plate Mul |+ x1 |+ x2
        plateAll (Div x1 x2) = plate Div |+ x1 |+ x2

 
instance Uniplate Stmt where
        uniplate = uniplateAll
 
instance (Typeable to, Uniplate to) => PlateAll Stmt to where
        plateAll (Assign x1 x2) = plate Assign |+ x1 |+ x2
        plateAll (Sequence x1) = plate Sequence |+ x1
        plateAll (If x1 x2 x3) = plate If |+ x1 |+ x2 |+ x3
        plateAll (While x1 x2) = plate While |+ x1 |+ x2
