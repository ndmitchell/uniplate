{-# LANGUAGE CPP, Rank2Types, MagicHash, UnboxedTuples, ExistentialQuantification, ScopedTypeVariables #-}

{- |
    Internal module, do not import or use.
-}

module Data.Generics.Uniplate.Internal.Data where

import Data.Generics.Str
import Data.Generics.Uniplate.Internal.Utils
import Data.Data
import Data.Generics
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


#elif 0


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
hitTestQuery from@(DataBox kfrom vfrom) kto = inlinePerformIO $ do
    mp <- readIORef hitTestCache
    let res = IntMap.lookup kfrom mp >>= IntMap.lookup kto
    case res of
        Just ans -> return ans
        Nothing -> do
            let res = toCache $ hitTestAdd from kto IntMap.empty
            res2 <- Control.Exception.catch (return $! res) (\(_ :: SomeException) -> return Nothing)
            -- -- uncomment these lines to see where type search fails
            -- if isNothing res2 then print ("failure",show (typeOf vfrom),kfrom,kto) else return ()

            atomicModifyIORef hitTestCache $ \mp -> flip (,) () $
                IntMap.insertWith (const $ IntMap.insert kto res2) kfrom (IntMap.singleton kto res2) mp
            return res2


-- need to classify each item as one of the following
data Res = RHit | RMiss | RFollow | RBad deriving (Show,Eq)


toCache :: IntMap Res -> Maybe Cache
toCache res | not $ IntSet.null $ f RBad = Nothing
            | otherwise = Just $ Cache (f RFollow) (f RMiss)
    where f x = IntMap.keysSet $ IntMap.filter (== x) res

hitTestAdd :: DataBox -> TypeKey -> IntMap Res -> IntMap Res
hitTestAdd from@(DataBox kfrom _) kto res = case sybChildren from of
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
sybChildren :: DataBox -> Maybe [DataBox]
sybChildren (DataBox k x)
    | k == typeRational = Just [dataBox (0 :: Integer)]
    | isAlgType dtyp = Just $ concatMap f ctrs
    | isNorepType dtyp = Nothing
    | otherwise = Just []
    where
        f ctr = gmapQ dataBox (asTypeOf (fromConstr ctr) x)
        ctrs = dataTypeConstrs dtyp
        dtyp = dataTypeOf x

typeRational = typeKey (undefined :: Rational)

#else

hitTest from to =
    let kto = typeKey to
    in case readCacheFollower (dataBox from) kto of
           Nothing -> Oracle $ \on -> if typeKey on == kto then Hit $ unsafeCoerce on else Follow
           Just test -> Oracle $ \on -> let kon = typeKey on in
                   if kon == kto then Hit $ unsafeCoerce on
                   else if test kon then Follow
                   else Miss



---------------------------------------------------------------------
-- CACHE
-- Store and compute the Follower and HitMap

data Cache = Cache HitMap (IntMap2 (Maybe Follower))

-- Indexed by the @from@ type, then the @to@ type
-- Nothing means that we can't perform the trick on the set
{-# NOINLINE cache #-}
cache :: IORef Cache
cache = unsafePerformIO $ newIORef $ Cache emptyHitMap IntMap.empty


readCacheFollower :: DataBox -> TypeKey -> Maybe Follower
readCacheFollower from@(DataBox kfrom vfrom) kto = inlinePerformIO $ do
    Cache hit follow <- readIORef cache
    case lookup2 kfrom kto follow of
        Just ans -> return ans
        Nothing -> do
            res <- Control.Exception.catch (return $! Just $! insertHitMap from hit) (\(_ :: SomeException) -> return Nothing)
            (hit,fol) <- return $ case res of
                Nothing -> (hit, Nothing)
                Just hit -> (hit, Just $ follower kfrom kto hit)
            -- -- uncomment these lines to see where type search fails
            -- if isNothing fol then print ("failure",show (typeOf vfrom),kfrom,kto) else return ()

            atomicModifyIORef cache $ \(Cache _ follow) -> (Cache hit (insert2 kfrom kto fol follow), ())
            return fol


-- from which values, what can you reach
readCacheHitMap :: DataBox -> Maybe HitMap
readCacheHitMap from@(DataBox kfrom vfrom) = inlinePerformIO $ do
    Cache hit _ <- readIORef cache
    case IntMap.lookup kfrom hit of
        Just _ -> return $ Just hit
        Nothing -> do
            res <- Control.Exception.catch (return $! Just $! insertHitMap from hit) (\(_ :: SomeException) -> return Nothing)
            case res of
                Nothing -> return Nothing
                Just hit -> do
                    atomicModifyIORef cache $ \(Cache _ follow) -> (Cache hit follow, ())
                    return $ Just hit


---------------------------------------------------------------------
-- INTMAP2

type IntMap2 a = IntMap (IntMap a)

lookup2 :: Int -> Int -> IntMap (IntMap x) -> Maybe x
lookup2 x y mp = IntMap.lookup x mp >>= IntMap.lookup y

insert2 :: Int -> Int -> x -> IntMap (IntMap x) -> IntMap (IntMap x)
insert2 x y v mp = IntMap.insertWith (const $ IntMap.insert y v) x (IntMap.singleton y v) mp


---------------------------------------------------------------------
-- FOLLOWER
-- Function to test if you should follow

type Follower = TypeKey -> Bool


-- HitMap must have addHitMap on the key
follower :: TypeKey -> TypeKey -> HitMap -> Follower
follower from to mp
    | IntSet.null hit = const False
    | IntSet.null miss = const True
    | otherwise = \now -> now `IntSet.member` hit
    where
        (hit,miss) = IntSet.partition (\x -> to `IntSet.member` grab x) (IntSet.insert from $ grab from)
        grab x = IntMap.findWithDefault (error "couldn't grab in follower") x mp


---------------------------------------------------------------------
-- DATA/TYPEABLE OPERATIONS

type TypeKey = Int

typeKey :: Typeable a => a -> Int
typeKey x = inlinePerformIO $ typeRepKey $ typeOf x


-- | An existential box representing a type which supports SYB
-- operations.
data DataBox = forall a . (Data a) => DataBox {dataBoxKey :: TypeKey, dataBoxVal :: a}

dataBox :: Data a => a -> DataBox
dataBox x = DataBox (typeKey x) x


-- NOTE: This function is partial, but all exceptions are caught later on
sybChildren :: Data a => a -> [DataBox]
sybChildren x
    | isAlgType dtyp = concatMap f ctrs
    | isNorepType dtyp = error "sybChildren on NorepType"
    | otherwise = []
    where
        f ctr = gmapQ dataBox (asTypeOf (fromConstr ctr) x)
        ctrs = dataTypeConstrs dtyp
        dtyp = dataTypeOf x


---------------------------------------------------------------------
-- HITMAP
-- What is the transitive closure of a type key

type HitMap = IntMap IntSet

emptyHitMap :: HitMap
emptyHitMap = IntMap.fromList
        [(tRational, IntSet.singleton tInteger)
        ,(tInteger, IntSet.empty)]
    where tRational = typeKey (undefined :: Rational)
          tInteger = typeKey (0 :: Integer)


insertHitMap :: DataBox -> HitMap -> HitMap
insertHitMap box hit = fixEq trans (populate box) `IntMap.union` hit
    where
        -- create a fresh box with all the necessary children that aren't in hit
        populate :: DataBox -> HitMap
        populate x = f x IntMap.empty
            where
                f (DataBox key val) mp
                    | key `IntMap.member` hit || key `IntMap.member` mp = mp
                    | otherwise = fs cs $ IntMap.insert key (IntSet.fromList $ map dataBoxKey cs) mp
                        where cs = sybChildren val

                fs [] mp = mp
                fs (x:xs) mp = fs xs (f x mp)


        -- update every one to be the transitive closure
        trans :: HitMap -> HitMap
        trans mp = IntMap.map f mp
            where
                f x = IntSet.unions $ x : map g (IntSet.toList x)
                g x = IntMap.findWithDefault (hit IntMap.! x) x mp


fixEq :: Eq a => (a -> a) -> a -> a
fixEq f x = if x == x2 then x2 else fixEq f x2
    where x2 = f x


#endif


---------------------------------------------------------------------
-- INSTANCE FUNCTIONS

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


---------------------------------------------------------------------
-- FUSION

data Transformer = forall a . Data a => Transformer TypeKey (a -> a)

transformer :: forall a . Data a => (a -> a) -> Transformer
transformer = Transformer (typeKey (undefined :: a))


-- | Note, you will increase performance if partially applying with the list of transformers
transformBis :: forall a . Data a => [[Transformer]] -> a -> a


#if __GLASGOW_HASKELL__ >= 606

-- basic algorithm:
-- as you go down, given transformBis [fN..f1]
--   if x is not in the set reachable by fN..f1, return x
--   if x is in the reachable set, gmap (transformBis [fN..f1]) x
--   if x is one of fN..f1, pick the lowest fi then
--      transformBis [fN..f(i+1)] $ fi $ gmap (transformBis [fi..f1]) x

transformBis ts | isJust hitBoxM = op (sliceMe 1 n)
    where
        on = dataBox (undefined :: a)
        hitBoxM = readCacheHitMap on
        hitBox = fromJust hitBoxM
        univ = IntSet.toAscList $ IntSet.insert (dataBoxKey on) $ hitBox IntMap.! dataBoxKey on
        n = length ts

        -- (a,b), where a < b, and both in range 1..n
        sliceMe i j = fromMaybe IntMap.empty $ lookup2 i j slices
        slices :: IntMap2 (IntMap (Maybe Transformer))
        slices = IntMap.fromAscList
            [ (i, IntMap.fromAscList [(j, slice i j ts) | (j,ts) <- zip [i..n] (tail $ inits ts)])
            | (i,ts) <- zip [1..n] (tails $ reverse ts)]

        slice :: Int -> Int -> [[Transformer]] -> IntMap (Maybe Transformer)
        slice from to tts = self
            where
                self = f IntMap.empty (zip [from..] tts) -- FIXME: flattening out here gives different results...
                f a ((i,[Transformer tk tr]):ts)
                    | tk `IntMap.member` a = f a ts
                    | otherwise = f (IntMap.insert tk t a) ts
                    where
                        t = Just $ Transformer tk $ op (sliceMe (i+1) to) . tr . gmapT (op $ sliceMe from i)

                f a [] = a `IntMap.union` IntMap.fromAscList (mapMaybe (g $ IntMap.keysSet a) $ univ)

                g a t = if b then Nothing else Just (t, Nothing)
                    where b = IntSet.null $ a `IntSet.intersection` (hitBox IntMap.! t)

        op :: forall b . Data b => IntMap (Maybe Transformer) -> b -> b
        op slice = case IntMap.lookup (typeKey (undefined :: b)) slice of
            Nothing -> id
            Just Nothing -> gmapT (op slice)
            Just (Just (Transformer _ t)) -> unsafeCoerce . t . unsafeCoerce

#endif


transformBis [] = id
transformBis ([]:xs) = transformBis xs
transformBis ((Transformer _ t:x):xs) = everywhere (mkT t) . transformBis (x:xs)

