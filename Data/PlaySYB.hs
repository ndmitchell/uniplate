{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module Data.PlaySYB(module Data.PlayEx, module Data.PlaySYB) where

import Data.PlayEx
import Data.Generics
import Data.Maybe
import Control.Monad.State


instance (Data a, Typeable a) => Play a where
    replaceChildren x = fromCC (collect_generate x)
    
    getChildren x = concat (gmapQ getChildrenEx x)
    


instance (Data a, Play b, Typeable a, Typeable b) => PlayEx a b where
    replaceChildrenEx x = fromCC (collect_generate_self x)

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


newtype C x a = C {fromC :: CC x a}

type CC x a = ([x], [x] -> (a, [x]))


fromCC :: CC x a -> ([x], [x] -> a)
fromCC (a, b) = (a, \i -> fst (b i))


collect_generate_self :: (Data on, Play with, Typeable on, Typeable with) => on -> CC with on
collect_generate_self x = res
        where
            res = case asTypeOf (cast x) (Just $ head $ fst res) of
                       Just y -> ([y], \(x:xs) -> (fromJust (cast x), xs))
                       Nothing -> collect_generate x


collect_generate :: (Data on, Play with, Typeable on, Typeable with) => on -> CC with on
collect_generate item = fromC $ gfoldl combine create item
    where
        -- forall a b . Data a => C with (a -> b) -> a -> C with b
        combine (C (c,g)) x = case collect_generate_self x of
                                  (c2, g2) -> C (c2 ++ c, regen g2)
            where
                regen g2 i = case g2 i of
                            (x2,i2) -> case g i2 of
                                (y2,i3) -> (y2 x2, i3)
        
        -- forall g . g -> C with g
        create x = C ([], \res -> (x, res))
