{-# OPTIONS_GHC -fglasgow-exts #-}

-- this module should not be imported directly

module Data.Generics.PlayMPTC(module Data.Generics.PlayEx, module Data.Generics.PlayMPTC) where

import Data.Generics.PlayEx


instance (Play with, PlayEx on with) => PlayEx [on] with where
    replaceType x = (concat currents, zipWith ($) generates . divide currents)
        where
            divide [] [] = []
            divide (x:xs) ys = y1 : divide xs y2
                where (y1,y2) = splitAt (length x) ys

            (currents, generates) = unzip $ map replaceType x
