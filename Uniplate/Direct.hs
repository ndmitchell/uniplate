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
        uniplate x = plate x

 
instance Uniplate Stmt where
        uniplate (Sequence x1) = plate Sequence ||* x1
        uniplate (While x1 x2) = plate (While x1) |* x2
        uniplate x = plate x

 
instance Biplate Stmt Expr where
        biplate (Assign x1 x2) = plate (Assign x1) |* x2
        biplate (Sequence x1) = plate Sequence ||+ x1
        biplate (While x1 x2) = plate While |* x1 |+ x2

 
instance Biplate Stmt [Stmt] where
        biplate (Sequence x1) = plate Sequence |* x1
        biplate (While x1 x2) = plate (While x1) |+ x2
        biplate x = plate x

 
instance Biplate Stmt Stmt where
        biplate = plateSelf

 
instance Biplate (Either String Int) Int where
        biplate (Right x1) = plate Right |* x1
        biplate x = plate x

 
instance Biplate (Either String Int) Char where
        biplate (Left x1) = plate Left ||* x1
        biplate x = plate x

 
instance Uniplate [Stmt] where
        uniplate ((:) x1 x2) = plate (:) |+ x1 |* x2
        uniplate x = plate x

 
instance Biplate [([Char], Int)] Int where
        biplate ((:) x1 x2) = plate (:) |+ x1 ||+ x2
        biplate x = plate x

 
instance Biplate ([Char], Int) Int where
        biplate (x1, x2) = plate ((,) x1) |* x2
