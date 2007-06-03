{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}

-- this module should not be imported directly

module Data.Generics.PlateDirect(
    module Data.Generics.Biplate,
    PlateAll(..), plate, (|+), (|-), (|*),
    PlateOne(..), plateSelf,
    (||+), (||*)
    ) where

import Data.Generics.Biplate
import Data.Generics.PlateInternal
import Data.Maybe


instance (Uniplate b, PlateAll a b) => Biplate a b where
    replaceType x = liftType $ plateAll x

instance PlateOne a => Uniplate a where
    replaceChildren x = liftType $ plateOne x


type Type from to = ([to] -> [to], [to] -> (from,[to]))


liftType :: Type from to -> ([to], [to] -> from)
liftType (a,b) = (a [], fst . b)


-- | Children are defined as the top-most items of type to
--   /starting beneath the root/.
--
--   This class should only be constructed with 'plate', '|+' and '|-'
class PlateAll from to where
    plateAll :: from -> Type from to

class PlateOne to where
    plateOne :: to -> Type to to


plate :: from -> Type from to
plate f = (id, \xs -> (f,xs))


(|*) :: Type (to -> from) to -> to -> Type from to
(|*) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        collect2 = collectL . (item:)
        generate2 xs = case generateL xs of
                        (a,(b:xs)) -> (a b, xs)


(|+) :: PlateAll item to => Type (item -> from) to -> item -> Type from to
(|+) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        (collectR,generateR) = plateAll item
        collect2 = collectL . collectR
        generate2 xs = case generateL xs of
                        (a,xs) -> case generateR xs of
                         (b,xs) -> (a b, xs)


(|-) :: Type (item -> from) to -> item -> Type from to
(|-) (collect,generate) item = (collect,\xs -> case generate xs of (r,xs) -> (r item, xs))


(||*) :: Type ([to] -> from) to -> [to] -> Type from to
(||*) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        collect2 = collectL . (item++)
        generate2 xs = case generateL xs of
                        (a,xs) -> let (x1,x2) = splitAt (length item) xs
                                  in (a x1,x2)


(||+) :: PlateAll item to => Type ([item] -> from) to -> [item] -> Type from to
(||+) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        (collectR,generateR) = plateListDiff item
        collect2 = collectL . collectR
        generate2 xs = case generateL xs of
                        (a,xs) -> case generateR xs of
                         (b,xs) -> (a b, xs)


plateListDiff [] = plate []
plateListDiff (x:xs) = plate (:) |+ x ||+ xs



plateSelf x = ((x:), \(x:xs) -> (x,xs))
