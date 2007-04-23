{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module Data.Generics.PlayData(
    module Data.Generics.PlayEx
    ) where

import Data.Generics.PlayEx
import Data.Generics.PlayInternal
import Data.Generics
import Data.Maybe
import Data.List
import qualified Data.IntSet as IntSet
import Control.Monad.State
import Debug.Trace



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


instance (Data a, Typeable a) => Play a where
    replaceChildren x = fromCC (collect_generate x)
    
    getChildren = \x -> concatCont (gmapQ (getTypeOneWith (fromBox answer)) x) []
        where
            answer :: Box a
            answer = containsMatch (undefined :: a) (undefined :: a)
    


instance (Data a, Data b, Play b, Typeable a, Typeable b) => PlayEx a b where
    replaceType x = fromCC (collect_generate_self x)

    getType = \x -> getTypeOneWith (fromBox answer) x []
        where
            answer :: Box b
            answer = containsMatch (undefined :: a) (undefined :: b)


getTypeOneWith :: (Data a, Typeable a, Typeable b) =>
                  (forall a . Typeable a => a -> Answer b) -> a -> [b] -> [b]
getTypeOneWith f a b = case f a of
                           Hit i -> i:b
                           Follow -> concatCont (gmapQ (getTypeOneWith f) a) b
                           Miss -> b



newtype C x a = C {fromC :: CC x a}

type CC x a = ([x] -> [x], [x] -> (a, [x]))


fromCC :: CC x a -> ([x], [x] -> a)
fromCC (a, b) = (a [], \i -> fst (b i))


collect_generate_self :: (Data on, Play with, Typeable on, Typeable with) => on -> CC with on
collect_generate_self x = res
        where
            res = case asTypeOf (cast x) (Just $ head $ fst res []) of
                       Just y -> ((y:), \(x:xs) -> (unsafeCast x, xs))
                       Nothing -> collect_generate x


collect_generate :: (Data on, Play with, Typeable on, Typeable with) => on -> CC with on
collect_generate item = fromC $ gfoldl combine create item
    where
        -- forall a b . Data a => C with (a -> b) -> a -> C with b
        combine (C (c,g)) x = case collect_generate_self x of
                                  (c2, g2) -> C (c2 . c, regen g2)
            where
                regen g2 i = case g2 i of
                            (x2,i2) -> case g i2 of
                                (y2,i3) -> (y2 x2, i3)
        
        -- forall g . g -> C with g
        create x = C (id, \res -> (x, res))





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
