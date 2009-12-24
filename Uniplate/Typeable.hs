{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses #-}
module Uniplate.Typeable where
import Data.Generics.Uniplate.Typeable
#include "CommonInc.hs"

 
instance (Typeable to, Uniplate to) => PlateAll Expr to where
        plateAll (Val x1) = plate Val |+ x1
        plateAll (Var x1) = plate Var |+ x1
        plateAll (Neg x1) = plate Neg |+ x1
        plateAll (Add x1 x2) = plate Add |+ x1 |+ x2

 
instance (Typeable to, Uniplate to) => PlateAll Stmt to where
        plateAll (Assign x1 x2) = plate Assign |+ x1 |+ x2
        plateAll (Sequence x1) = plate Sequence |+ x1
        plateAll (While x1 x2) = plate While |+ x1 |+ x2
