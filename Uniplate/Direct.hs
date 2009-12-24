{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses #-}
module Uniplate.Direct where
import Data.Generics.Uniplate.Direct
#include "CommonInc.hs"

 
instance Uniplate Expr where
        uniplate (Neg x1) = plate Neg |* x1
        uniplate (Add x1 x2) = plate Add |* x1 |* x2
        uniplate (Sub x1 x2) = plate Sub |* x1 |* x2
        uniplate (Mul x1 x2) = plate Mul |* x1 |* x2
        uniplate (Div x1 x2) = plate Div |* x1 |* x2
        uniplate x = plate x

 
instance Uniplate Stmt where
        uniplate (Sequence x1) = plate Sequence ||* x1
        uniplate (If x1 x2 x3) = plate (If x1) |* x2 |* x3
        uniplate (While x1 x2) = plate (While x1) |* x2
        uniplate x = plate x
