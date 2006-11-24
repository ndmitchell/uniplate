{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.PlayEx(module Data.Play, module Data.PlayEx) where

import Data.Play
import Control.Monad


class Play with => PlayEx on with where
    replaceChildrenEx :: on -> ([with], [with] -> on)


playExDefault :: (Play on, PlayEx on with) => on -> ([with], [with] -> on)
playExDefault x = (concat currents, generate . zipWith ($) generates . divide currents)
    where
        divide [] [] = []
        divide (x:xs) ys = y1 : divide xs y2
            where (y1,y2) = splitAt (length x) ys
    
        (currents, generates) = unzip $ map replaceChildrenEx current
        (current, generate) = replaceChildren x


playSelf :: a -> ([a], [a] -> a)
playSelf x = ([x], \[x] -> x)


playMore :: PlayEx a b => (a -> c) -> a -> ([b],[b] -> c)
playMore part item = (current, part . generate)
    where (current, generate) = replaceChildrenEx item


mapUnderEx :: PlayEx on with => (with -> with) -> on -> on
mapUnderEx f x = generate $ map (mapUnder f) current
    where (current, generate) = replaceChildrenEx x

mapUnderExM :: (Monad m, PlayEx on with) => (with -> m with) -> on -> m on
mapUnderExM f x = liftM generate $ mapM (mapUnderM f) current
    where (current, generate) = replaceChildrenEx x


allOverEx :: PlayEx on with => on -> [with]
allOverEx = concatMap allOver . fst . replaceChildrenEx


instance (Play with, PlayEx on with) => PlayEx [on] with where
    replaceChildrenEx x = (concat currents, zipWith ($) generates . divide currents)
        where
            divide [] [] = []
            divide (x:xs) ys = y1 : divide xs y2
                where (y1,y2) = splitAt (length x) ys

            (currents, generates) = unzip $ map replaceChildrenEx x

