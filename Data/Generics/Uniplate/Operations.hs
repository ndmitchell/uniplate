{-# LANGUAGE CPP, MultiParamTypeClasses #-}
{- |
Definitions of 'Uniplate' and 'Biplate' classes, along with all the standard operations.

Import this module directly only if you are defining new Uniplate operations, otherwise
import one of "Data.Generics.Uniplate.Direct", "Data.Generics.Uniplate.Typeable" or
"Data.Generics.Uniplate.Data".

Most functions have an example of a possible use for the function.
To illustate, I have used the @Expr@ type as below:

> data Expr = Val Int
>           | Neg Expr
>           | Add Expr Expr
-}

module Data.Generics.Uniplate.Operations where

#include "Internal/OperationsInc.hs"
