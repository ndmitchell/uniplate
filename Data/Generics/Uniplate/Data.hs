{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, Rank2Types, CPP,
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

{- |
    This module exports 'Biplate' instances for everything with 'Data' defined.
    Using GHC the 'Data' instances can be constructed with @deriving Data@.
-}
module Data.Generics.Uniplate.Data(
    module Data.Generics.Uniplate.Operations
    ) where

import Data.Generics.Uniplate.Operations

#include "Internal/DataInc.hs"
