{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module Data.PlaySYB(module Data.PlayEx, module Data.PlaySYB) where

import Data.PlayEx
import Data.Generics
import Data.Maybe
import Control.Monad.State


instance (Data a, Typeable a) => Play a where
    replaceChildren = collect_generate
    
    getChildren = concat . gmapQ getChildrenEx
    


instance (Data a, Play b, Typeable a, Typeable b) => PlayEx a b where
    replaceChildrenEx = collect_generate_self

    getChildrenEx x = res
        where
            res = case asTypeOf (cast x) (Just $ head res) of
                       Just y -> [y]
                       Nothing -> concat $ gmapQ getChildrenEx x


{-
OLD VERSION USING TWO SEPARATE TRAVERSALS

collect_generate :: (Data on, Play with, Typeable on, Typeable with) => on -> ([with],[with] -> on)
collect_generate item = (collect, generate)
    where
        collect = concat $ gmapQ getChildrenEx item

        generate xs = evalState (gmapM f item) xs
            where
                f x = do
                        ys <- get
                        let (as,bs) = splitAt (length col) ys
                        put bs
                        return $ gen as
                    where
                        (col,gen) = replaceChildrenEx x
-}


newtype C x a = C {fromC :: ([x], [x] -> a)}



collect_generate_self :: (Data on, Play with, Typeable on, Typeable with) => on -> ([with], [with] -> on)
collect_generate_self x = res
        where
            res = case asTypeOf (cast x) (Just $ head $ fst res) of
                       Just y -> ([y], \[x] -> fromJust (cast x))
                       Nothing -> collect_generate x


collect_generate :: (Data on, Play with, Typeable on, Typeable with) => on -> ([with], [with] -> on)
collect_generate item = fromC $ gfoldl combine create item
    where
        -- forall a b . Data a => C with (a -> b) -> a -> C with b
        combine (C (c,g)) x = C (c2 ++ c, \i -> let (a,b) = splitAt (length c2) i in g b (g2 a))
            where (c2,g2) = collect_generate_self x
        
        -- forall g . g -> C with g
        create x = C ([], \[] -> x)
