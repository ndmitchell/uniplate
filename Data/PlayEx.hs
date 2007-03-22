{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.PlayEx(module Data.Play, module Data.PlayEx) where

import Data.Play
import Data.PlayOn
import Control.Monad


-- * The Class

class Play to => PlayEx from to where
    replaceType :: ReplaceType from to
    
    getType :: from -> [to]
    getType = fst . replaceType


-- * The Combinators

playExDefault :: (Play on, PlayEx on with) => on -> ([with], [with] -> on)
playExDefault x = (concat currents, generate . zipWith ($) generates . divide currents)
    where
        divide [] [] = []
        divide (x:xs) ys = y1 : divide xs y2
            where (y1,y2) = splitAt (length x) ys
    
        (currents, generates) = unzip $ map replaceType current
        (current, generate) = replaceChildren x


playSelf :: a -> ([a], [a] -> a)
playSelf x = ([x], \[x] -> x)


playMore :: PlayEx a b => (a -> c) -> a -> ([b],[b] -> c)
playMore part item = (current, part . generate)
    where (current, generate) = replaceType item


play :: on -> ([with],[with] -> on)
play f = ([], \[] -> f)


(/\) :: PlayEx item with => ([with], [with] -> item -> on) -> item -> ([with], [with] -> on)
(/\) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        (collectR,generateR) = replaceType item
        collect2 = collectL ++ collectR
        generate2 xs = generateL a (generateR b)
            where (a,b) = splitAt (length collect2) xs


(/\!) :: ([with], [with] -> item -> on) -> item -> ([with], [with] -> on)
(/\!) (collect,generate) item = (collect,\xs -> generate xs item)


-- * The Operations

traverseEx :: PlayEx from to => (to -> to) -> from -> from
traverseEx = traverseOn replaceType


traverseExM :: (Monad m, PlayEx from to) => (to -> m to) -> from -> m from
traverseExM = traverseOnM replaceType


descendEx :: PlayEx from to => (to -> to) -> from -> from
descendEx = descendOn replaceType


descendExM :: (Monad m, PlayEx from to) => (to -> m to) -> from -> m from
descendExM = descendOnM replaceType


everythingEx :: PlayEx from to => from -> [to]
everythingEx = concatMap everything . getType


everythingContextEx :: PlayEx from to => from -> [(to, to -> from)]
everythingContextEx = everythingContextOn replaceType
