{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, ExistentialQuantification #-}

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


instance (Data a, Typeable a) => Uniplate a where
    replaceChildren = \x -> fromCC (collect_generate (fromBox answer) x)
        where
            answer :: Box a
            answer = containsMatch (undefined :: a) (undefined :: a)


instance (Data a, Data b, Uniplate b, Typeable a, Typeable b) => Biplate a b where
    replaceType = \x -> fromCC (collect_generate_self (fromBox answer) x)
        where
            answer :: Box b
            answer = containsMatch (undefined :: a) (undefined :: b)


newtype C x a = C {fromC :: CC x a}

type CC x a = ([x] -> [x], [x] -> (a, [x]))


fromCC :: CC x a -> ([x], [x] -> a)
fromCC (a, b) = (a [], \i -> fst (b i))


collect_generate_self :: (Data on, Uniplate with, Typeable on, Typeable with) =>
                         (forall a . Typeable a => a -> Answer with) -> on -> CC with on
collect_generate_self oracle x = res
        where
            res = case oracle x of
                       Hit y -> ((y:), \(x:xs) -> (unsafeCast x, xs))
                       Follow -> collect_generate oracle x
                       Miss -> (id, \res -> (x,res))


collect_generate :: (Data on, Uniplate with, Typeable on, Typeable with) =>
                    (forall a . Typeable a => a -> Answer with) -> on -> CC with on
collect_generate oracle item = fromC $ gfoldl combine create item
    where
        -- forall a b . Data a => C with (a -> b) -> a -> C with b
        combine (C (c,g)) x = case collect_generate_self oracle x of
                                  (c2, g2) -> C (c . c2, regen g2)
            where
                regen g2 i = case g i of
                            (x2,i2) -> case g2 i2 of
                                (y2,i3) -> (x2 y2, i3)
        
        -- forall g . g -> C with g
        create x = C (id, \res -> (x, res))





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
