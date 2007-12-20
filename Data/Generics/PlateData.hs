{-# OPTIONS_GHC -fglasgow-exts -cpp -fallow-undecidable-instances #-}
{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, ExistentialQuantification, Rank2Types, CPP #-}
-- OPTIONS_GHC is required only for 6.4.2, not 6.6.1

{- |
    This module exports 'Biplate' instances for everything with 'Data' defined.
    Using GHC the 'Data' instances can be constructed with @deriving Data@.
-}


module Data.Generics.PlateData(
    module Data.Generics.Biplate
    ) where

import Data.Generics.Biplate
import Data.Generics.PlateInternal
import Data.Generics
import Data.Maybe
import Data.List
import qualified Data.IntSet as IntSet
import Control.Monad.State



-- | An existential box representing a type which supports SYB
-- operations.
data DataBox = forall a . (Typeable a, Data a) => DataBox a

data Box find = Box {fromBox :: forall a . Typeable a => a -> Answer find}

data Answer a = Hit {fromHit :: a} -- you just hit the element you were after (here is a cast)
              | Follow -- go forward, you will find something
              | Miss -- you failed to sink my battleship!



containsMatch :: (Data start, Typeable start, Data find, Typeable find) =>
                 start -> find ->
                 Box find

#if __GLASGOW_HASKELL__ < 606
-- GHC 6.4.2 does not export typeRepKey, so we can't do the trick
-- as efficiently, so we just give up and revert to always following

containsMatch start find = Box query
    where
        query a = case cast a of
                       Just y -> Hit y
                       Nothing -> Follow

#else
-- GHC 6.6 does contain typeRepKey, so only follow when appropriate

containsMatch start find = Box query
    where
        typeInt x = inlinePerformIO $ typeRepKey x
    
        query :: Typeable a => a -> Answer find
        query a = if tifind == tia then Hit (unsafeCast a)
                  else if tia `IntSet.member` timatch then Follow else Miss
            where tia = typeInt $ typeOf a
    
        tifind = typeInt tfind
        timatch = IntSet.fromList $ map typeInt tmatch

        tfind = typeOf find
        tmatch = f [tfind] (filter ((/=) tfind . fst) $ containsList start)

        f want have = if null want2 then [] else want2 ++ f want2 no
            where
                want2 = map fst yes
                (yes,no) = partition (not . null . intersect want . snd) have

containsList :: (Data a, Typeable a) => a -> [(TypeRep, [TypeRep])]
containsList x = f [] [DataBox x]
    where
        f done [] = []
        f done (DataBox t:odo)
            | tt `elem` done = f done odo
            | otherwise = (tt,map (\(DataBox a) -> typeOf a) xs) : f (tt:done) (xs++odo)
            where
                tt = typeOf t
                xs = contains t

contains :: (Data a, Typeable a) => a -> [DataBox]
contains x = if isAlgType dtyp then concatMap f ctrs else []
    where
        f ctr = gmapQ DataBox (asTypeOf (fromConstr ctr) x)
        ctrs = dataTypeConstrs dtyp
        dtyp = dataTypeOf x

#endif


instance (Data a, Typeable a) => Uniplate a where
    uniplate = \x -> fromCC (collect_generate (fromBox answer) x)
        where
            answer :: Box a
            answer = containsMatch (undefined :: a) (undefined :: a)


instance (Data a, Data b, Uniplate b, Typeable a, Typeable b) => Biplate a b where
    biplate = \x -> fromCC (collect_generate_self (fromBox answer) x)
        where
            answer :: Box b
            answer = containsMatch (undefined :: a) (undefined :: b)


newtype C x a = C {fromC :: CC x a}

type CC x a = (Str x, Str x -> a)


fromCC :: CC x a -> (Str x, Str x -> a)
fromCC = id


collect_generate_self :: (Data on, Data with, Typeable on, Typeable with) =>
                         (forall a . Typeable a => a -> Answer with) -> on -> CC with on
collect_generate_self oracle x = res
        where
            res = case oracle x of
                       Hit y -> (One y, \(One x) -> unsafeCast x)
                       Follow -> collect_generate oracle x
                       Miss -> (Zero, \_ -> x)


collect_generate :: (Data on, Data with, Typeable on, Typeable with) =>
                    (forall a . Typeable a => a -> Answer with) -> on -> CC with on
collect_generate oracle item = fromC $ gfoldl combine create item
    where
        -- forall a b . Data a => C with (a -> b) -> a -> C with b
        combine (C (c,g)) x = case collect_generate_self oracle x of
                                  (c2, g2) -> C (Two c c2, \(Two c' c2') -> g c' (g2 c2'))

        -- forall g . g -> C with g
        create x = C (Zero, \_ -> x)





{-
OLD VERSION USING TWO SEPARATE TRAVERSALS

collect_generate :: (Data on, Uniplate with, Typeable on, Typeable with) => on -> ([with],[with] -> on)
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
