{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

-- this module should not be imported directly

module Data.Generics.PlayTypeable(
    module Data.Generics.PlayEx,
    module Data.Generics.PlayTypeable
    ) where

import Data.Generics.PlayEx
import Data.Typeable
import Data.Maybe


instance (Typeable a, Typeable b, Play b, PlayAll a b) => PlayEx a b where
    replaceType x = res
        where
            res = case asTypeOf (cast x) (Just $ head $ fst res) of
                      Nothing -> replaceAll x
                      Just y -> ([y], \[y] -> fromJust $ cast y)




-- | Children are defined as the top-most items of type to
--   /starting beneath the root/.
--
--   This class should only be constructed with 'play', '|+' and '|-'
class PlayAll from to where
    replaceAll :: ReplaceType from to
    
    getAll :: from -> [to]
    getAll = fst . replaceAll



playSelf :: a -> ([a], [a] -> a)
playSelf x = ([x], \[x] -> x)


play :: on -> ([with],[with] -> on)
play f = ([], \[] -> f)


(|+) :: PlayEx item with => ([with], [with] -> item -> on) -> item -> ([with], [with] -> on)
(|+) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        (collectR,generateR) = replaceType item
        collect2 = collectL ++ collectR
        generate2 xs = generateL a (generateR b)
            where (a,b) = splitAt (length collect2) xs


(|-) :: ([with], [with] -> item -> on) -> item -> ([with], [with] -> on)
(|-) (collect,generate) item = (collect,\xs -> generate xs item)

