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
import Data.IORef
import Control.Applicative
import Control.Exception
import Control.Monad
import System.Environment(getEnv)
import qualified Data.IntMap as IntMap; import Data.IntMap(IntMap)


#if __GLASGOW_HASKELL__ < 606

---------------------------------------------------------------------
-- GHC 6.4 and below

import qualified Data.Set as Set
import qualified Data.Map as Map

type TypeKey = TypeRep
type TypeSet = Set.Set TypeKey
type TypeMap = Map.Map TypeKey

typeKey :: Typeable a => a -> TypeKey
typeKey = typeOf

#elif __GLASGOW_HASKELL__ < 702

---------------------------------------------------------------------
-- GHC 6.6 to 7.0 (has typeRepKey)

import qualified Data.IntSet as Set
import qualified Data.IntMap as Map

type TypeKey = Int
type TypeSet = Set.IntSet
type TypeMap = Map.IntMap

typeKey :: Typeable a => a -> TypeKey
typeKey x = inlinePerformIO $ typeRepKey $ typeOf x

#else

---------------------------------------------------------------------
-- GHC 7.2 and above (using fingerprint)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
type TypeSet = Set.HashSet TypeKey
type TypeMap = Map.HashMap TypeKey

type TypeKey = TypeRep

typeKey :: Typeable a => a -> TypeKey
typeKey = typeOf

#endif


#if __GLASGOW_HASKELL__ < 702

---------------------------------------------------------------------
-- GHC 7.0 and below (using containers API)

(!) = (Map.!)
map_findWithDefault = Map.findWithDefault
map_fromAscList = Map.fromAscList
map_keysSet = Map.keysSet
map_member = Map.member
set_partition = Set.partition
set_toAscList = Set.toAscList
set_unions = Set.unions

#else

---------------------------------------------------------------------
-- GHC 7.2 and above (using unordered-containers API)

(!) mp k = map_findWithDefault (error "Could not find element") k mp
map_findWithDefault d k mp = fromMaybe d $ Map.lookup k mp -- in 0.2.3.0 lookupDefault is strict in the default :(
map_fromAscList = Map.fromList
map_keysSet = Set.fromList . Map.keys
map_member x xs = isJust $ Map.lookup x xs
set_partition f x = (Set.filter f x, Set.filter (not . f) x)
set_toAscList = Set.toList
set_unions = foldr Set.union Set.empty

#endif


{-# NOINLINE uniplateVerbose #-}
uniplateVerbose :: Int -- -1 = error if failed, 0 = quiet, 1 = print errors only, 2 = print everything
uniplateVerbose = unsafePerformIO $ do
    fmap read (getEnv "UNIPLATE_VERBOSE") `Control.Exception.catch` \(_ :: SomeException) -> return 0


---------------------------------------------------------------------
-- HIT TEST


data Answer a = Hit {fromHit :: a} -- you just hit the element you were after (here is a cast)
              | Follow -- go forward, you will find something
              | Miss -- you failed to sink my battleship!

data Oracle to = Oracle {fromOracle :: forall on . Typeable on => on -> Answer to}

{-# INLINE hitTest #-}
hitTest :: (Data from, Data to) => from -> to -> Oracle to
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

data Cache = Cache HitMap (TypeMap2 (Maybe Follower))

-- Indexed by the @from@ type, then the @to@ type
-- Nothing means that we can't perform the trick on the set
{-# NOINLINE cache #-}
cache :: IORef Cache
cache = unsafePerformIO $ newIORef $ Cache emptyHitMap Map.empty


readCacheFollower :: DataBox -> TypeKey -> Maybe Follower
readCacheFollower from@(DataBox kfrom vfrom) kto = inlinePerformIO $ do
    Cache hit follow <- readIORef cache
    case lookup2 kfrom kto follow of
        Just ans -> return ans
        Nothing -> do
            res <- Control.Exception.try (return $! insertHitMap from hit)
            (hit,fol) <- return $ case res of
                Left _ -> (hit, Nothing)
                Right hit -> (hit, Just $ follower kfrom kto hit)

            let msg =
                    "# Uniplate lookup on (" ++ show (typeOf vfrom) ++ "), from (" ++ show kfrom ++ "), to (" ++ show kto ++ "): " ++
                    either (\(msg::SomeException) -> "FAILURE (" ++ show msg ++ ")") (const "Success") res

            when (uniplateVerbose + maybe 1 (const 0) fol >= 2) $ putStrLn msg
            when (uniplateVerbose < 0 && isNothing fol) $ error msg

            atomicModifyIORef cache $ \(Cache _ follow) -> (Cache hit (insert2 kfrom kto fol follow), ())
            return fol


-- from which values, what can you reach
readCacheHitMap :: DataBox -> Maybe HitMap
readCacheHitMap from@(DataBox kfrom vfrom) = inlinePerformIO $ do
    Cache hit _ <- readIORef cache
    case Map.lookup kfrom hit of
        Just _ -> return $ Just hit
        Nothing -> do
            res <- Control.Exception.catch (return $! Just $! insertHitMap from hit) (\(_ :: SomeException) -> return Nothing)
            case res of
                Nothing -> return Nothing
                Just hit -> do
                    atomicModifyIORef cache $ \(Cache _ follow) -> (Cache hit follow, ())
                    return $ Just hit


---------------------------------------------------------------------
-- TYPEMAP2/INTMAP2

type TypeMap2 a = TypeMap (TypeMap a)

lookup2 :: TypeKey -> TypeKey -> TypeMap2 a -> Maybe a
lookup2 x y mp = Map.lookup x mp >>= Map.lookup y

insert2 :: TypeKey -> TypeKey -> a -> TypeMap2 a -> TypeMap2 a
insert2 x y v mp = Map.insertWith (const $ Map.insert y v) x (Map.singleton y v) mp


type IntMap2 a = IntMap (IntMap a)

intLookup2 :: Int -> Int -> IntMap2 a -> Maybe a
intLookup2 x y mp = IntMap.lookup x mp >>= IntMap.lookup y

intInsert2 :: Int -> Int -> a -> IntMap2 a -> IntMap2 a
intInsert2 x y v mp = IntMap.insertWith (const $ IntMap.insert y v) x (IntMap.singleton y v) mp


---------------------------------------------------------------------
-- FOLLOWER
-- Function to test if you should follow

type Follower = TypeKey -> Bool


-- HitMap must have addHitMap on the key
follower :: TypeKey -> TypeKey -> HitMap -> Follower
follower from to mp
    | Set.null hit = const False
    | Set.null miss = const True
    | Set.size hit < Set.size miss = \k -> k `Set.member` hit
    | otherwise = \k -> not $ k `Set.member` miss
    where
        (hit,miss) = set_partition (\x -> to `Set.member` grab x) (Set.insert from $ grab from)
        grab x = map_findWithDefault (error "couldn't grab in follower") x mp


---------------------------------------------------------------------
-- DATA/TYPEABLE OPERATIONS

-- | An existential box representing a type which supports SYB
-- operations.
data DataBox = forall a . (Data a) => DataBox {dataBoxKey :: TypeKey, dataBoxVal :: a}

dataBox :: Data a => a -> DataBox
dataBox x = DataBox (typeKey x) x


-- NOTE: This function is partial, but all exceptions are caught later on
sybChildren :: Data a => a -> [DataBox]
sybChildren x
    | isAlgType dtyp = concatMap f ctrs
    | isNorepType dtyp = []
        -- Extensive discussions with Lennart and Roman decided that if something returns NorepType, it really wants to be atomic
        -- so we should let it be, and pretend it has no children.
        -- The most common types which say this are Data.Set/Data.Map, and we think that's a bug in their Data instances.
        -- error $ "Data.Generics.Uniplate.Data: sybChildren on data type which returns NorepType, " ++ show (typeOf x) ++ ", " ++ show dtyp
    | otherwise = []
    where
        f ctr = gmapQ dataBox (asTypeOf (fromConstr ctr) x)
        ctrs = dataTypeConstrs dtyp
        dtyp = dataTypeOf x


---------------------------------------------------------------------
-- HITMAP
-- What is the transitive closure of a type key

type HitMap = TypeMap TypeSet

emptyHitMap :: HitMap
emptyHitMap = Map.fromList
        [(tRational, Set.singleton tInteger)
        ,(tInteger, Set.empty)]
    where tRational = typeKey (undefined :: Rational)
          tInteger = typeKey (0 :: Integer)


insertHitMap :: DataBox -> HitMap -> HitMap
insertHitMap box hit = fixEq trans (populate box) `Map.union` hit
    where
        -- create a fresh box with all the necessary children that aren't in hit
        populate :: DataBox -> HitMap
        populate x = f x Map.empty
            where
                f (DataBox key val) mp
                    | key `map_member` hit || key `map_member` mp = mp
                    | otherwise = fs cs $ Map.insert key (Set.fromList $ map dataBoxKey cs) mp
                        where cs = sybChildren val

                fs [] mp = mp
                fs (x:xs) mp = fs xs (f x mp)


        -- update every one to be the transitive closure
        trans :: HitMap -> HitMap
        trans mp = Map.map f mp
            where
                f x = set_unions $ x : map g (Set.toList x)
                g x = map_findWithDefault (hit ! x) x mp


fixEq :: Eq a => (a -> a) -> a -> a
fixEq f x = if x == x2 then x2 else fixEq f x2
    where x2 = f x


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

descendDataM :: (Data on, Applicative m) => (forall a . Typeable a => a -> Answer on) -> (on -> m on) -> on -> m on
descendDataM oracle op = gmapA (descendBiDataM oracle op)

descendBiDataM :: (Data on, Data with, Applicative m) => (forall a . Typeable a => a -> Answer with) -> (with -> m with) -> on -> m on
descendBiDataM oracle op x = case oracle x of
    Hit y -> unsafeCoerce $ op y
    Follow -> gmapA (descendBiDataM oracle op) x
    Miss -> pure x

gmapA :: forall m a. (Data a, Applicative m) => (forall d. Data d => d -> m d) -> a -> m a
gmapA f = gfoldl k pure
    where k :: Data d => m (d -> b) -> d -> m b
          k c x = c <*> f x


---------------------------------------------------------------------
-- FUSION

data Transformer = forall a . Data a => Transformer TypeKey (a -> a)


-- | Wrap up a @(a -> a)@ transformation function, to use with 'transformBis'
transformer :: Data a => (a -> a) -> Transformer
transformer = transformer_


-- Don't export directly, as don't want Haddock to see the forall
transformer_ :: forall a . Data a => (a -> a) -> Transformer
transformer_ = Transformer (typeKey (undefined :: a))


-- | Apply a sequence of transformations in order. This function obeys the equivalence:
--
-- > transformBis [[transformer f],[transformer g],...] == transformBi f . transformBi g . ...
--
--   Each item of type @[Transformer]@ is applied in turn, right to left. Within each
--   @[Transformer]@, the individual @Transformer@ values may be interleaved.
--
--   The implementation will attempt to perform fusion, and avoid walking any part of the
--   data structure more than necessary. To further improve performance, you may wish to
--   partially apply the first argument, which will calculate information about the relationship
--   between the transformations.
transformBis :: forall a . Data a => [[Transformer]] -> a -> a
transformBis = transformBis_


transformBis_ :: forall a . Data a => [[Transformer]] -> a -> a

-- basic algorithm:
-- as you go down, given transformBis [fN..f1]
--   if x is not in the set reachable by fN..f1, return x
--   if x is in the reachable set, gmap (transformBis [fN..f1]) x
--   if x is one of fN..f1, pick the lowest fi then
--      transformBis [fN..f(i+1)] $ fi $ gmap (transformBis [fi..f1]) x

transformBis_ ts | isJust hitBoxM = op (sliceMe 1 n)
    where
        on = dataBox (undefined :: a)
        hitBoxM = readCacheHitMap on
        hitBox = fromJust hitBoxM
        univ = set_toAscList $ Set.insert (dataBoxKey on) $ hitBox ! dataBoxKey on
        n = length ts

        -- (a,b), where a < b, and both in range 1..n
        sliceMe i j = fromMaybe Map.empty $ intLookup2 i j slices
        slices :: IntMap2 (TypeMap (Maybe Transformer))
        slices = IntMap.fromAscList
            [ (i, IntMap.fromAscList [(j, slice i j ts) | (j,ts) <- zip [i..n] (tail $ inits ts)])
            | (i,ts) <- zip [1..n] (tails $ reverse ts)]

        slice :: Int -> Int -> [[Transformer]] -> TypeMap (Maybe Transformer)
        slice from to tts = self
            where
                self = f Map.empty (zip [from..] tts) -- FIXME: flattening out here gives different results...
                f a ((i,[Transformer tk tr]):ts)
                    | tk `map_member` a = f a ts
                    | otherwise = f (Map.insert tk t a) ts
                    where
                        t = Just $ Transformer tk $ op (sliceMe (i+1) to) . tr . gmapT (op $ sliceMe from i)

                f a [] = a `Map.union` map_fromAscList (mapMaybe (g $ map_keysSet a) univ)

                g a t = if b then Nothing else Just (t, Nothing)
                    where b = Set.null $ a `Set.intersection` (hitBox ! t)

        op :: forall b . Data b => TypeMap (Maybe Transformer) -> b -> b
        op slice = case Map.lookup (typeKey (undefined :: b)) slice of
            Nothing -> id
            Just Nothing -> gmapT (op slice)
            Just (Just (Transformer _ t)) -> unsafeCoerce . t . unsafeCoerce


transformBis_ [] = id
transformBis_ ([]:xs) = transformBis_ xs
transformBis_ ((Transformer _ t:x):xs) = everywhere (mkT t) . transformBis_ (x:xs)
