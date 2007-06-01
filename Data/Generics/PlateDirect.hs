{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}

-- this module should not be imported directly

module Data.Generics.PlateDirect(
    module Data.Generics.PlayEx,
    PlayAll(..), play, (|+), (|-), (|*),
    PlayOne(..), playSelf,
    (||+), (||*)
    ) where

import Data.Generics.PlayEx
import Data.Generics.PlayInternal
import Data.Maybe


instance (Play b, PlayAll a b) => PlayEx a b where
    replaceType x = liftType $ playAll x

instance PlayOne a => Play a where
    replaceChildren x = liftType $ playOne x


type Type from to = ([to] -> [to], [to] -> (from,[to]))


liftType :: Type from to -> ([to], [to] -> from)
liftType (a,b) = (a [], fst . b)


-- | Children are defined as the top-most items of type to
--   /starting beneath the root/.
--
--   This class should only be constructed with 'play', '|+' and '|-'
class PlayAll from to where
    playAll :: from -> Type from to

class PlayOne to where
    playOne :: to -> Type to to


play :: from -> Type from to
play f = (id, \xs -> (f,xs))


(|*) :: Type (to -> from) to -> to -> Type from to
(|*) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        collect2 = collectL . (item:)
        generate2 xs = case generateL xs of
                        (a,(b:xs)) -> (a b, xs)


(|+) :: PlayAll item to => Type (item -> from) to -> item -> Type from to
(|+) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        (collectR,generateR) = playAll item
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


(||+) :: PlayAll item to => Type ([item] -> from) to -> [item] -> Type from to
(||+) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        (collectR,generateR) = playListDiff item
        collect2 = collectL . collectR
        generate2 xs = case generateL xs of
                        (a,xs) -> case generateR xs of
                         (b,xs) -> (a b, xs)


playListDiff [] = play []
playListDiff (x:xs) = play (:) |+ x ||+ xs



playSelf x = ((x:), \(x:xs) -> (x,xs))
