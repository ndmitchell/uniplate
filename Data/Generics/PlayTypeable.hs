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
    replaceType x = playMore x


replaceChildrenAll a = playAll a


type Type from to = ([to], [to] -> from)


playMore :: (Typeable from, Typeable to, PlayAll from to) => from -> Type from to
playMore x = res
    where
        res = case asTypeOf (cast x) (Just $ head $ fst res) of
                  Nothing -> playAll x
                  Just y -> ([y], \(y:_) -> fromJust $ cast y)


-- | Children are defined as the top-most items of type to
--   /starting beneath the root/.
--
--   This class should only be constructed with 'play', '|+' and '|-'
class PlayAll from to where
    playAll :: from -> Type from to


play :: from -> Type from to
play f = ([], \_ -> f)


(|+) :: (Typeable item, Typeable to, PlayAll item to) => Type (item -> from) to -> item -> Type from to
(|+) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        (collectR,generateR) = playMore item
        collect2 = collectL ++ collectR
        generate2 xs = generateL a (generateR b)
            where (a,b) = splitAt (length collectL) xs


(|-) :: Type (item -> from) to -> item -> Type from to
(|-) (collect,generate) item = (collect,\xs -> generate xs item)


instance (PlayAll from to, Typeable from, Typeable to, Play to) => PlayAll [from] to where
    playAll x = case x of
        [] -> play []
        (x:xs) -> play (:) |+ x |+ xs
