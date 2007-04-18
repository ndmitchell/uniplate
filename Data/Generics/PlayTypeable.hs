{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

-- this module should not be imported directly

module Data.Generics.PlayTypeable(module Data.Generics.PlayEx, module Data.Generics.PlayTypeable) where

import Data.Generics.PlayEx
import Data.Typeable
import Data.Maybe


instance (Typeable a, Typeable b, Play a, Play b, PlayAll a b) => PlayEx a b where
    replaceType x = res
        where
            res = case asTypeOf (cast x) (Just $ head $ fst res) of
                      Nothing -> replaceAll x
                      Just y -> ([y], \[y] -> fromJust $ cast y)
