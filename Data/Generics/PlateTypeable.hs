{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}

-- this module should not be imported directly

module Data.Generics.PlateTypeable(
    module Data.Generics.Biplate,
    module Data.Typeable,
    replaceChildrenAll, play, (|+), (|-), PlayAll(..)
    ) where

import Data.Generics.Biplate
import Data.Generics.PlateInternal
import Data.Typeable
import Data.Maybe


instance (Typeable a, Typeable b, Uniplate b, PlayAll a b) => Biplate a b where
    replaceType x = liftType $ playMore x


replaceChildrenAll :: PlayAll a b => a -> ([b],[b] -> a)
replaceChildrenAll a = liftType $ playAll a



type Type from to = ([to] -> [to], [to] -> (from,[to]))


liftType :: Type from to -> ([to], [to] -> from)
liftType (a,b) = (a [], fst . b)


playMore :: (Typeable from, Typeable to, PlayAll from to) => from -> Type from to
playMore x = res
    where
        res = case asTypeOf (cast x) (Just $ head $ fst res []) of
                  Nothing -> playAll x
                  Just y -> ((y:), \(y:ys) -> (unsafeCast y, ys))


-- | Children are defined as the top-most items of type to
--   /starting beneath the root/.
--
--   This class should only be constructed with 'play', '|+' and '|-'
class PlayAll from to where
    playAll :: from -> Type from to


play :: from -> Type from to
play f = (id, \xs -> (f,xs))


(|+) :: (Typeable item, Typeable to, PlayAll item to) => Type (item -> from) to -> item -> Type from to
(|+) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        (collectR,generateR) = playMore item
        collect2 = collectL . collectR
        generate2 xs = case generateL xs of
                        (a,xs) -> case generateR xs of
                         (b,xs) -> (a b, xs)


(|-) :: Type (item -> from) to -> item -> Type from to
(|-) (collect,generate) item = (collect,\xs -> case generate xs of (r,xs) -> (r item, xs))


instance (PlayAll from to, Typeable from, Typeable to, Uniplate to) => PlayAll [from] to where
    playAll x = case x of
        [] -> play []
        (x:xs) -> play (:) |+ x |+ xs
