{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, Rank2Types, CPP,
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

{- |
    This module defines 'Uniplate' / 'Biplate' instances for every type with a
    'Data' instance. Using GHC, Data can be derived automatically with:

    > data Expr = Var Int | Neg Expr | Add Expr Expr
    >             deriving (Data,Typeable)

    All the Uniplate operations defined in "Data.Generics.Uniplate.Operations"
    can be used. If you are working with abstract data types, such as @Map@ or @Set@
    from the @containers@ package, you  may also need to use the data types defined
    in "Data.Generics.Uniplate.Data.Instances".

    For faster performance (5x faster, but requires writing instances) switch to
    "Data.Generics.Uniplate.Direct". If you get instance conflicts
    when using both @Data@ and @Direct@, switch to "Data.Generics.Uniplate.DataOnly".

    The instances are faster than GHC because they precompute a table of useful information,
    then use this information when performing the traversals. Sometimes it is not possible
    to compute the table, in which case this library will perform about the same speed as
    SYB.

    Setting the environment variable @$UNIPLATE_VERBOSE@ has the following effects:

  * @-1@ - raise a program error every time construction of the table fails

  * @0@ (or unset) - never print any messages or raise any errors

  * @1@ - give a message every time a table is computed

  * @2@ - give a message when table computation fails

    The @$UNIPLATE_VERBOSE@ environment variable must be set before the first call to uniplate.
-}
module Data.Generics.Uniplate.Data(
    module Data.Generics.Uniplate.Operations,
    transformBis, Transformer, transformer
    ) where

import Data.Generics.Uniplate.Operations

#include "Internal/DataInc.hs"
