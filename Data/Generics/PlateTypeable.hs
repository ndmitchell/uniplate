{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}

-- this module should not be imported directly

module Data.Generics.PlateTypeable(
    module Data.Generics.Biplate,
    module Data.Typeable,
    replaceChildrenAll, plate, (|+), (|-), PlateAll(..)
    ) where

import Data.Generics.Biplate
import Data.Generics.PlateInternal
import Data.Typeable
import Data.Maybe


instance (Typeable a, Typeable b, Uniplate b, PlateAll a b) => Biplate a b where
    replaceType x = liftType $ plateMore x


replaceChildrenAll :: PlateAll a b => a -> ([b],[b] -> a)
replaceChildrenAll a = liftType $ plateAll a



type Type from to = ([to] -> [to], [to] -> (from,[to]))


liftType :: Type from to -> ([to], [to] -> from)
liftType (a,b) = (a [], fst . b)


plateMore :: (Typeable from, Typeable to, PlateAll from to) => from -> Type from to
plateMore x = res
    where
        res = case asTypeOf (cast x) (Just $ head $ fst res []) of
                  Nothing -> plateAll x
                  Just y -> ((y:), \(y:ys) -> (unsafeCast y, ys))


-- | Children are defined as the top-most items of type to
--   /starting beneath the root/.
--
--   This class should only be constructed with 'plate', '|+' and '|-'
class PlateAll from to where
    plateAll :: from -> Type from to


plate :: from -> Type from to
plate f = (id, \xs -> (f,xs))


(|+) :: (Typeable item, Typeable to, PlateAll item to) => Type (item -> from) to -> item -> Type from to
(|+) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        (collectR,generateR) = plateMore item
        collect2 = collectL . collectR
        generate2 xs = case generateL xs of
                        (a,xs) -> case generateR xs of
                         (b,xs) -> (a b, xs)


(|-) :: Type (item -> from) to -> item -> Type from to
(|-) (collect,generate) item = (collect,\xs -> case generate xs of (r,xs) -> (r item, xs))


instance (PlateAll from to, Typeable from, Typeable to, Uniplate to) => PlateAll [from] to where
    plateAll x = case x of
        [] -> plate []
        (x:xs) -> plate (:) |+ x |+ xs
