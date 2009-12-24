{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, Rank2Types, CPP,
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

{- |
    This module exports 'Biplate' instances for everything with 'Data' defined.
    Using GHC the 'Data' instances can be constructed with @deriving Data@.
-}
module Data.Generics.Uniplate.Data(
    module Data.Generics.Uniplate.Classes
    ) where

import Data.Generics.Uniplate.Classes

#include "Internal/DataInc.hs"
