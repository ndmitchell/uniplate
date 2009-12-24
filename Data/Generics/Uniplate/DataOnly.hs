{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, Rank2Types, CPP,
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

{- |
    This module exports 'Biplate' instances for everything with 'Data' defined.
    Using GHC the 'Data' instances can be constructed with @deriving Data@.
-}
module Data.Generics.Uniplate.DataOnly(
    module Data.Generics.Uniplate.Internal.DataOnlyClasses
    ) where

import Data.Generics.Uniplate.Internal.DataOnlyClasses

#include "Internal/DataInc.hs"
