{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

-- this module should not be imported directly

module Data.Generics.PlayTypeable(
    module Data.Generics.PlayEx,
    module Data.Generics.PlayTypeable,
    module Data.Typeable
    ) where

import Data.Generics.PlayEx
import Data.Typeable
import Data.Maybe


instance (Typeable a, Typeable b, Play b, PlayAll a b) => PlayEx a b where
    replaceType x = res
        where
            res = case asTypeOf (cast x) (Just $ head $ fst res) of
                      Nothing -> playAll x
                      Just y -> ([y], \(y:_) -> fromJust $ cast y)


replaceChildrenAll a = playAll a


type Type from to = ([to], [to] -> from)


-- | Children are defined as the top-most items of type to
--   /starting beneath the root/.
--
--   This class should only be constructed with 'play', '|+' and '|-'
class PlayAll from to where
    playAll :: from -> Type from to


play :: from -> Type from to
play f = ([], \_ -> f)


(|+) :: PlayEx item from => Type (item -> to) from -> item -> Type to from
(|+) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        (collectR,generateR) = replaceType item
        collect2 = collectL ++ collectR
        generate2 xs = generateL a (generateR b)
            where (a,b) = splitAt (length collectL) xs


(|-) :: Type (item -> to) from -> item -> Type to from
(|-) (collect,generate) item = (collect,\xs -> generate xs item)


instance (PlayAll from to, Typeable from, Typeable to, Play to) => PlayAll [from] to where
    playAll x = case x of
        [] -> play []
        (x:xs) -> play (:) |+ x |+ xs
