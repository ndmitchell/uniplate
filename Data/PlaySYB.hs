{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module Data.PlaySYB(module Data.PlayEx, module Data.PlaySYB) where

import Data.PlayEx
import Data.Generics
import Control.Monad.State


instance (Data a, Typeable a) => Play a where
    replaceChildren = collect_generate


instance (Data a, Play b, Typeable a, Typeable b) => PlayEx a b where
    replaceChildrenEx item = (collect, generate)
        where
            equal = typeOf collect == typeOf [item]
            coerce x = let Just y = cast x in y
            collect = if equal then [coerce item] else collect2
            generate = if equal then \[x] -> coerce x else generate2
            
            (collect2,generate2) = collect_generate item


collect_generate :: (Data on, Play with, Typeable on, Typeable with) => on -> ([with],[with] -> on)
collect_generate item = (collect, generate)
    where
        collect = execState (gmapM f item) []
            where
                f x = modify (++ extra) >> return x
                    where extra = fst $ replaceChildrenEx x

        generate xs = evalState (gmapM f item) xs
            where
                f x = do
                        ys <- get
                        let (as,bs) = splitAt (length col) ys
                        put bs
                        return $ gen as
                    where
                        (col,gen) = replaceChildrenEx x

