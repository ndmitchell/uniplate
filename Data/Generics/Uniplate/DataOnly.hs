{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, Rank2Types, CPP,
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

{- |
    This module functions identically to "Data.Generics.Uniplate.Data", but instead of
    using the standard 'Uniplate' / 'Biplate' classes defined in
    "Data.Generics.Uniplate.Operations" it uses a local copy.

    Only use this module if you are using both @Data@ and @Direct@ instances in
    the same project and they are conflicting.
-}
module Data.Generics.Uniplate.DataOnly(
    module Data.Generics.Uniplate.Internal.DataOnlyOperations,
    transformBis, Transformer, transformer
    ) where

import Data.Generics.Uniplate.Internal.DataOnlyOperations

#include "Internal/DataInc.hs"
