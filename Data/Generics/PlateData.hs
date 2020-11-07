{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, ExistentialQuantification, Rank2Types #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
    /DEPRECATED/: Use "Data.Generics.Uniplate.Data" instead.

    This module exports 'Biplate' instances for everything with 'Data' defined.
    Using GHC the 'Data' instances can be constructed with @deriving Data@.
-}
module Data.Generics.PlateData
    {-# DEPRECATED "Use Data.Generics.Uniplate.Data instead" #-}
    (
    module Data.Generics.Biplate
    ) where

import Data.Generics.Biplate
import Data.Generics.Uniplate.Internal.Utils
import Data.Generics


data Box find = Box {fromBox :: forall a . Typeable a => a -> Answer find}

data Answer a = Hit {_fromHit :: a} -- you just hit the element you were after (here is a cast)
              | Follow -- go forward, you will find something
              | Miss -- you failed to sink my battleship!



containsMatch :: (Data start, Typeable start, Data find, Typeable find) =>
                 start -> find ->
                 Box find

-- GHC 6.4.2 does not export typeRepKey, so we can't do the trick
-- as efficiently, so we just give up and revert to always following

containsMatch start find = Box query
    where
        query a = case cast a of
                       Just y -> Hit y
                       Nothing -> Follow


instance (Data a, Typeable a) => Uniplate a where
    uniplate = collect_generate (fromBox answer)
        where
            answer :: Box a
            answer = containsMatch (undefined :: a) (undefined :: a)


instance (Data a, Data b, Uniplate b, Typeable a, Typeable b) => Biplate a b where
    biplate = collect_generate_self (fromBox answer)
        where
            answer :: Box b
            answer = containsMatch (undefined :: a) (undefined :: b)


newtype C x a = C {fromC :: CC x a}

type CC x a = (Str x, Str x -> a)


collect_generate_self :: (Data on, Data with, Typeable on, Typeable with) =>
                         (forall a . Typeable a => a -> Answer with) -> on -> CC with on
collect_generate_self oracle x = res
        where
            res = case oracle x of
                       Hit y -> (One y, \(One x) -> unsafeCoerce x)
                       Follow -> collect_generate oracle x
                       Miss -> (Zero, \_ -> x)


collect_generate :: (Data on, Data with, Typeable on, Typeable with) =>
                    (forall a . Typeable a => a -> Answer with) -> on -> CC with on
collect_generate oracle item = fromC $ gfoldl combine create item
    where
        -- forall a b . Data a => C with (a -> b) -> a -> C with b
        combine (C (c,g)) x = case collect_generate_self oracle x of
                                  (c2, g2) -> C (Two c c2, \(Two c' c2') -> g c' (g2 c2'))

        -- forall g . g -> C with g
        create x = C (Zero, \_ -> x)
