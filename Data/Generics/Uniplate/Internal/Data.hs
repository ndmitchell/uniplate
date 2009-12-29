{-# LANGUAGE CPP, Rank2Types, MagicHash, UnboxedTuples, ExistentialQuantification, ScopedTypeVariables #-}

{- |
    Internal module, do not import or use.
-}

module Data.Generics.Uniplate.Internal.Data where

import Data.Generics.Str
import Data.Generics.Uniplate.Internal.Utils
import Data.Data
import Data.Maybe
import Data.List
import qualified Data.IntSet as IntSet
import Data.IntSet(IntSet)
import qualified Data.IntMap as IntMap
import Data.IntMap(IntMap)
import Data.IORef
import Control.Exception


---------------------------------------------------------------------
-- HIT TEST


data Answer a = Hit {fromHit :: a} -- you just hit the element you were after (here is a cast)
              | Follow -- go forward, you will find something
              | Miss -- you failed to sink my battleship!

data Oracle to = Oracle {fromOracle :: forall on . Typeable on => on -> Answer to}

{-# INLINE hitTest #-}
hitTest :: (Data from, Data to) => from -> to -> Oracle to


#if __GLASGOW_HASKELL__ < 606
-- GHC 6.4.2 does not export typeRepKey, so we can't do the trick
-- as efficiently, so we just give up and revert to always following

hitTest _ _ = Oracle . maybe Follow Hit . cast


#else


hitTest from to =
    let kto = typeKey to
    in case hitTestQuery (dataBox from) kto of
           Nothing -> Oracle $ \on -> if typeKey on == kto then Hit $ unsafeCoerce on else Follow
           Just cache -> let test = cacheHitTest cache in
               Oracle $ \on -> let kon = typeKey on in
                   if kon == kto then Hit $ unsafeCoerce on
                   else if test kon then Follow
                   else Miss


-- A cache hit test, but partially evaluated
{-# INLINE cacheHitTest #-}
cacheHitTest :: Cache -> TypeKey -> Bool
cacheHitTest (Cache hit miss)
    | IntSet.null hit = const False
    | IntSet.null miss = const True
    | otherwise = \x -> x `IntSet.member` hit


-- hit means that this value may result in a hit
-- miss means that this value will never result in a hit
data Cache = Cache {hit :: IntSet, miss :: IntSet} deriving Show
newCache = Cache IntSet.empty IntSet.empty


-- Indexed by the @from@ type, then the @to@ type
-- Nothing means that we can't perform the trick on the set
{-# NOINLINE hitTestCache #-}
hitTestCache :: IORef (IntMap (IntMap (Maybe Cache)))
hitTestCache = unsafePerformIO $ newIORef IntMap.empty


hitTestQuery :: DataBox -> TypeKey -> Maybe Cache
hitTestQuery from@(DataBox kfrom _) kto = inlinePerformIO $ do
    mp <- readIORef hitTestCache
    let res = IntMap.lookup kfrom mp >>= IntMap.lookup kto
    case res of
        Just ans -> return ans
        Nothing -> do
            let res = toCache $ hitTestAdd from kto IntMap.empty
            res2 <- Control.Exception.catch (return $! res) (\(_ :: SomeException) -> return Nothing)
            let mp2 = IntMap.adjust (IntMap.insert kto res2) kfrom mp
            writeIORef hitTestCache mp2
            return res2


-- need to classify each item as one of the following
data Res = RHit | RMiss | RFollow | RBad deriving (Show,Eq)


toCache :: IntMap Res -> Maybe Cache
toCache res | not $ IntSet.null $ f RBad = Nothing
            | otherwise = Just $ Cache (f RFollow) (f RMiss)
    where f x = IntMap.keysSet $ IntMap.filter (== x) res

hitTestAdd :: DataBox -> TypeKey -> IntMap Res -> IntMap Res
hitTestAdd (DataBox kfrom from) kto res = case sybChildren from of
    _ | kfrom `IntMap.member` res -> res
    Nothing -> IntMap.insert kfrom RBad res

    -- make an inductive hypothesis that this value is a miss
    -- if it turns out you were wrong, start again
    -- uses backtracking, so could be expensive
    Just xs | kto == kfrom -> hitTestAdds xs kto $ IntMap.insert kfrom RHit res
            | correct -> res2
            | otherwise -> hitTestAdds xs kto $ IntMap.insert kfrom RFollow res
        where res2 = hitTestAdds xs kto $ IntMap.insert kfrom RMiss res
              correct = all ((==) RMiss . (res2 IntMap.!) . dataBoxKey) xs

hitTestAdds :: [DataBox] -> TypeKey -> IntMap Res -> IntMap Res
hitTestAdds [] kto res = res
hitTestAdds (x:xs) kto res = hitTestAdds xs kto $ hitTestAdd x kto res


type TypeKey = Int

typeKey :: Typeable a => a -> Int
typeKey x = inlinePerformIO $ typeRepKey $ typeOf x


-- | An existential box representing a type which supports SYB
-- operations.
data DataBox = forall a . (Data a) => DataBox {dataBoxKey :: TypeKey, dataBoxVal :: a}

dataBox :: Data a => a -> DataBox
dataBox x = DataBox (typeKey x) x

-- return all the possible children of a node
-- if you can't do so, just return Nothing
sybChildren :: Data a => a -> Maybe [DataBox]
sybChildren x | isAlgType dtyp = Just $ concatMap f ctrs
              | isNorepType dtyp = Nothing
              | otherwise = Just []
    where
        f ctr = gmapQ dataBox (asTypeOf (fromConstr ctr) x)
        ctrs = dataTypeConstrs dtyp
        dtyp = dataTypeOf x

#endif


newtype C x a = C {fromC :: CC x a}

type CC x a = (Str x, Str x -> a)


biplateData :: (Data on, Data with) => (forall a . Typeable a => a -> Answer with) -> on -> CC with on
biplateData oracle x = case oracle x of
    Hit y -> (One y, \(One x) -> unsafeCoerce x)
    Follow -> uniplateData oracle x
    Miss -> (Zero, \_ -> x)


uniplateData :: forall on with . (Data on, Data with) => (forall a . Typeable a => a -> Answer with) -> on -> CC with on
uniplateData oracle item = fromC $ gfoldl combine create item
    where
        combine :: Data a => C with (a -> b) -> a -> C with b
        combine (C (c,g)) x = case biplateData oracle x of
                                  (c2, g2) -> C (Two c c2, \(Two c' c2') -> g c' (g2 c2'))

        create :: g -> C with g
        create x = C (Zero, \_ -> x)


descendData :: Data on => (forall a . Typeable a => a -> Answer on) -> (on -> on) -> on -> on
descendData oracle op = gmapT (descendBiData oracle op)

descendBiData :: (Data on, Data with) => (forall a . Typeable a => a -> Answer with) -> (with -> with) -> on -> on
descendBiData oracle op x = case oracle x of
    Hit y -> unsafeCoerce $ op y
    Follow -> gmapT (descendBiData oracle op) x
    Miss -> x
