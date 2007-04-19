{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Generics.PlayEx(module Data.Generics.Play, module Data.Generics.PlayEx) where

import Data.Generics.Play
import Data.Generics.PlayOn
import Data.Typeable
import Data.Maybe
import Control.Monad


-- * The Classes


-- | Children are defined as the top-most items of type to
--   /starting at the root/.
class (Typeable from, Typeable to, Play to) => PlayEx from to where
    replaceType :: ReplaceType from to
    
    getType :: from -> [to]
    getType = fst . replaceType


-- | Children are defined as the top-most items of type to
--   /starting beneath the root/.
class (Typeable from, Typeable to, Play to) => PlayAll from to where
    replaceAll :: ReplaceType from to
    
    getAll :: from -> [to]
    getAll = fst . replaceAll


instance PlayAll a b => PlayEx a b where
    replaceType x = res
        where
            res = case asTypeOf (cast x) (Just $ head $ fst res) of
                      Nothing -> replaceAll x
                      Just y -> ([y], \[y] -> fromJust $ cast y)


-- * The Combinators

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


-- * The Operations

traverseEx :: PlayEx from to => (to -> to) -> from -> from
traverseEx = traverseOn replaceType


traverseExM :: (Monad m, PlayEx from to) => (to -> m to) -> from -> m from
traverseExM = traverseOnM replaceType


rewriteEx :: PlayEx from to => (to -> Maybe to) -> from -> from
rewriteEx = rewriteOn replaceType


rewriteExM :: (Monad m, PlayEx from to) => (to -> m (Maybe to)) -> from -> m from
rewriteExM = rewriteOnM replaceType


descendEx :: PlayEx from to => (to -> to) -> from -> from
descendEx = descendOn replaceType


descendExM :: (Monad m, PlayEx from to) => (to -> m to) -> from -> m from
descendExM = descendOnM replaceType


everythingEx :: PlayEx from to => from -> [to]
everythingEx = concatMap everything . getType


everythingContextEx :: PlayEx from to => from -> [(to, to -> from)]
everythingContextEx = everythingContextOn replaceType
