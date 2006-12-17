{-# OPTIONS_GHC -fglasgow-exts #-}

-- this module should not be imported directly

module Data.PlayMPTC(module Data.PlayEx, module Data.PlayMPTC) where

import Data.PlayEx


instance (Play with, PlayEx on with) => PlayEx [on] with where
    replaceChildrenEx x = (concat currents, zipWith ($) generates . divide currents)
        where
            divide [] [] = []
            divide (x:xs) ys = y1 : divide xs y2
                where (y1,y2) = splitAt (length x) ys

            (currents, generates) = unzip $ map replaceChildrenEx x
