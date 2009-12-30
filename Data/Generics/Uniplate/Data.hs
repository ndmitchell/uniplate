{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, Rank2Types, CPP,
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

{- |
    This module defines 'Uniplate' / 'Biplate' instances for every type with a
    'Data' instance. Using GHC, Data can be derived automatically with:

    > data Expr = Var Int | Neg Expr | Add Expr Expr
    >             deriving (Data,Typeable)

    All the Uniplate operations defined in "Data.Generics.Uniplate.Operations"
    can be used.

    For faster performance (5x faster, but requires writing instances) switch to
    "Data.Generics.Uniplate.Direct". If you get instance conflicts
    when using both @Data@ and @Direct@, switch to "Data.Generics.Uniplate.DataOnly".
-}
module Data.Generics.Uniplate.Data(
    module Data.Generics.Uniplate.Operations
    ) where

import Data.Generics.Uniplate.Operations

#include "Internal/DataInc.hs"
