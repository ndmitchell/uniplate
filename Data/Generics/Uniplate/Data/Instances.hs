{-# LANGUAGE DeriveDataTypeable #-}

-- | In some cases, 'Data' instances for abstract types are incorrect,
--   and fail to work correctly with Uniplate. This module defines three helper
--   types ('Hide', 'Trigger' and 'Invariant') to assist when writing instances
--   for abstract types. The 'Hide' type is useful when you want to mark some part
--   of your data type as being ignored by "Data.Generics.Uniplate.Data"
--   (and any other 'Data' based generics libraries, such as @syb@).
--
--   Using the helper types, this module defines wrappers for types in
--   the @containers@ package, namely 'Map', 'Set', 'IntMap' and 'IntSet'.
--   The standard @containers@ 'Data' instances all treat the types as abstract,
--   but the wrapper types allow you to traverse within the data types, ensuring
--   the necessary invariants are maintained. In particular, if you do not modify
--   the keys reconstruct will be /O(n)/ instead of /O(n log n)/.
--
--   As an example of how to implement your own abstract type wrappers, the 'Map' data
--   type is defined as:
--
-- @
--   newtype Map k v = Map ('Invariant' ('Trigger' [k], 'Trigger' [v], Hide (Map.Map k v)))
--      deriving (Data, Typeable)
-- @
--
--   The 'Map' type is defined as an 'Invariant' of three components - the keys, the values, and
--   the underlying @Map@. We use 'Invariant' to ensure that the keys/values/map always remain in sync.
--   We use 'Trigger' on the keys and values to ensure that whenever the keys or values change we
--   rebuild the @Map@, but if they don't, we reuse the previous @Map@. The 'fromMap' function is
--   implemented by pattern matching on the 'Map' type:
--
-- @
--   'fromMap' ('Map' ('Invariant' _ (_,_,'Hide' x))) = x
-- @
--
--   The 'toMap' function is slightly harder, as we need to come up with an invariant restoring function:
--
-- > toMap :: Ord k => Map.Map k v -> Map k v
-- > toMap x = Map $ Invariant inv $ create x
-- >     where
-- >         create x = (Trigger False ks, Trigger False vs, Hide x)
-- >             where (ks,vs) = unzip $ Map.toAscList x
-- > 
-- >         inv (ks,vs,x)
-- >             | trigger ks = create $ Map.fromList $ zip (fromTrigger ks) (fromTrigger vs)
-- >             | trigger vs = create $ Map.fromDistinctAscList $ zip (fromTrigger ks) (fromTrigger vs)
-- >             | otherwise = (ks,vs,x)
--
--   The 'create' function creates a value from a @Map@, getting the correct keys and values. The 'inv'
--   function looks at the triggers on the keys/values. If the keys trigger has been tripped, then we
--   reconstruct the @Map@ using @fromList@. If the values trigger has been tripped, but they keys trigger
--   has not, we can use @fromDistinctAscList@, reducing the complexity of constructing the @Map@. If nothing
--   has changed we can reuse the previous value.
--
--   The end result is that all Uniplate (or @syb@) traversals over 'Map' result in a valid value, which has
--   had all appropriate transformations applied.
module Data.Generics.Uniplate.Data.Instances(
    Hide(..), Trigger(..), Invariant(..),
    Map, fromMap, toMap,
    Set, fromSet, toSet,
    IntMap, fromIntMap, toIntMap,
    IntSet, fromIntSet, toIntSet
    ) where

import Data.Data
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet


---------------------------------------------------------------------
-- DATA TYPES

-- | The 'Hide' data type has a 'Data' instance which reports having no constructors,
--   as though the type was defined as using the extension @EmptyDataDecls@:
--
-- > data Hide a
--
--   This type is suitable for defining regions that are avoided by Uniplate traversals.
--   As an example:
--
-- > transformBi (+1) (1, 2, Hide 3, Just 4) == (2, 3, Hide 3, Just 4)
--
--   As a result of having no constructors, any calls to the methods 'toConstr' or 'gunfold'
--   will raise an error.
newtype Hide a = Hide {fromHide :: a}
    deriving (Read,Ord,Eq,Typeable)

instance Show a => Show (Hide a) where
    show (Hide a) = "Hide " ++ show a

instance Functor Hide where
    fmap f (Hide x) = Hide $ f x


instance Typeable a => Data (Hide a) where
    gfoldl k z x = z x
    gunfold k z c = error "Data.Generics.Uniplate.Data.Instances.Hide: gunfold not implemented - data type has no constructors"
    toConstr _ = error "Data.Generics.Uniplate.Data.Instances.Hide: toConstr not implemented - data type has no constructors"
    dataTypeOf _ = tyHide

tyHide = mkDataType "Data.Generics.Uniplate.Data.Instances.Hide" []


-- | The 'Trigger' data type has a 'Data' instance which reports as being defined:
--
-- > data Trigger a = Trigger a
--
--   However, whenever a 'gfoldl' or 'gunfold' constructs a new value, it will have the
--   'trigger' field set to 'True'. The trigger information is useful to indicate whether
--   any invariants have been broken, and thus need fixing. As an example:
--
-- > data SortedList a = SortedList (Trigger [a]) deriving (Data,Typeable)
-- > toSortedList xs = SortedList $ Trigger False $ sort xs
-- > fromSortedList (SortedList (Trigger t xs)) = if t then sort xs else xs
--
--   This data type represents a sorted list. When constructed the items are initially sorted,
--   but operations such as 'gmapT' could break that invariant. The 'Trigger' type is used to
--   detect when the Data operations have been performed, and resort the list.
--
--   The 'Trigger' type is often used in conjunction with 'Invariant', which fixes the invariants.
data Trigger a = Trigger {trigger :: Bool, fromTrigger :: a}
    deriving (Read,Ord,Eq,Show,Typeable)

instance Functor Trigger where
    fmap f (Trigger a b) = Trigger a $ f b


instance (Data a, Typeable a) => Data (Trigger a) where
    gfoldl k z (Trigger _ x) = z (Trigger True) `k` x
    gunfold k z c = k $ z $ Trigger True
    toConstr Trigger{} = conTrigger
    dataTypeOf _ = tyTrigger

conTrigger = mkConstr tyTrigger "Trigger" [] Prefix
tyTrigger = mkDataType "Data.Generics.Uniplate.Data.Instances.Trigger" [conTrigger]


-- | The 'Invariant' data type as a 'Data' instance which reports as being defined:
--
-- > data Invariant a = Invariant a
--
--   However, whenever a 'gfoldl' constructs a new value, it will have the function in
--   the 'invariant' field applied to it. As an example:
--
-- > data SortedList a = SortedList (Invariant [a]) deriving (Data,Typeable)
-- > toSortedList xs = SortedList $ Invariant sort (sort xs)
-- > fromSortedList (SortedList (Invariant _ xs)) = xs
--
--   Any time an operation such as 'gmapT' is applied to the data type, the 'invariant' function
--   is applied to the result. The @fromSortedList@ function can then rely on this invariant.
--
--   The 'gunfold' method is partially implemented - all constructed values will have an undefined
--   value for all fields, regardless of which function is passed to 'fromConstrB'. If you only use
--   'fromConstr' (as Uniplate does) then the 'gunfold' method is sufficient.
data Invariant a = Invariant {invariant :: a -> a, fromInvariant :: a}
    deriving Typeable

instance Show a => Show (Invariant a) where
    show (Invariant _ x) = "Invariant " ++ show x

instance (Data a, Typeable a) => Data (Invariant a) where
    gfoldl k z (Invariant f x) = z (Invariant f . f) `k` x
    gunfold k z c = k $ z $ \x -> Invariant (error msg) (error msg `asTypeOf` x)
        where msg = "Data.Generics.Uniplate.Data.Instances.Invariant: gunfold only partially implemented"
    toConstr Invariant{} = conInvariant
    dataTypeOf _ = tyInvariant

conInvariant = mkConstr tyInvariant "Invariant" [] Prefix
tyInvariant = mkDataType "Data.Generics.Uniplate.Data.Instances.Invariant" [conInvariant]


---------------------------------------------------------------------
-- DATA TYPES

-- | Invariant preserving version of @Map@ from the @containers@ packages, suitable for use with 'Uniplate'.
--   Use 'toMap' to construct values, and 'fromMap' to deconstruct values.
newtype Map k v = Map (Invariant (Trigger [k], Trigger [v], Hide (Map.Map k v)))
    deriving (Data, Typeable)

instance (Show k, Show v) => Show (Map k v) where; show = show . fromMap
instance (Eq k, Eq v) => Eq (Map k v) where; a == b = fromMap a == fromMap b
instance (Ord k, Ord v) => Ord (Map k v) where; compare a b = compare (fromMap a) (fromMap b)

-- | Deconstruct a value of type 'Map'.
fromMap :: Map k v -> Map.Map k v
fromMap (Map (Invariant _ (_,_,Hide x))) = x

-- | Construct a value of type 'Map'.
toMap :: Ord k => Map.Map k v -> Map k v
toMap x = Map $ Invariant inv $ create x
    where
        create x = (Trigger False ks, Trigger False vs, Hide x)
            where (ks,vs) = unzip $ Map.toAscList x

        inv (ks,vs,x)
            | trigger ks = create $ Map.fromList $ zip (fromTrigger ks) (fromTrigger vs)
            | trigger vs = create $ Map.fromDistinctAscList $ zip (fromTrigger ks) (fromTrigger vs) -- recreate ks/vs to reduce memory usage
            | otherwise = (ks,vs,x)


-- | Invariant preserving version of @Set@ from the @containers@ packages, suitable for use with 'Uniplate'.
--   Use 'toSet' to construct values, and 'fromSet' to deconstruct values.
newtype Set k = Set (Invariant (Trigger [k], Hide (Set.Set k)))
    deriving (Data, Typeable)

instance Show k => Show (Set k) where; show = show . fromSet
instance Eq k => Eq (Set k) where; a == b = fromSet a == fromSet b
instance Ord k => Ord (Set k) where; compare a b = compare (fromSet a) (fromSet b)

-- | Deconstruct a value of type 'Set'.
fromSet :: Set k -> Set.Set k
fromSet (Set (Invariant _ (_,Hide x))) = x

-- | Construct a value of type 'Set'.
toSet :: Ord k => Set.Set k -> Set k
toSet x = Set $ Invariant inv $ create x
    where
        create x = (Trigger False $ Set.toList x, Hide x)

        inv (ks,x)
            | trigger ks = create $ Set.fromList $ fromTrigger ks
            | otherwise = (ks,x)


-- | Invariant preserving version of @IntMap@ from the @containers@ packages, suitable for use with 'Uniplate'.
--   Use 'toIntMap' to construct values, and 'fromIntMap' to deconstruct values.
newtype IntMap v = IntMap (Invariant (Trigger [Int], Trigger [v], Hide (IntMap.IntMap v)))
    deriving (Data, Typeable)

instance Show v => Show (IntMap v) where; show = show . fromIntMap
instance Eq v => Eq (IntMap v) where; a == b = fromIntMap a == fromIntMap b
instance Ord v => Ord (IntMap v) where; compare a b = compare (fromIntMap a) (fromIntMap b)

-- | Deconstruct a value of type 'IntMap'.
fromIntMap :: IntMap v -> IntMap.IntMap v
fromIntMap (IntMap (Invariant _ (_,_,Hide x))) = x

-- | Construct a value of type 'IntMap'.
toIntMap :: IntMap.IntMap v -> IntMap v
toIntMap x = IntMap $ Invariant inv $ create x
    where
        create x = (Trigger False ks, Trigger False vs, Hide x)
            where (ks,vs) = unzip $ IntMap.toAscList x

        inv (ks,vs,x)
            | trigger ks = create $ IntMap.fromList $ zip (fromTrigger ks) (fromTrigger vs)
            | trigger vs = create $ IntMap.fromDistinctAscList $ zip (fromTrigger ks) (fromTrigger vs) -- recreate ks/vs to reduce memory usage
            | otherwise = (ks,vs,x)


-- | Invariant preserving version of @IntSet@ from the @containers@ packages, suitable for use with 'Uniplate'.
--   Use 'toIntSet' to construct values, and 'fromIntSet' to deconstruct values.
newtype IntSet = IntSet (Invariant (Trigger [Int], Hide (IntSet.IntSet)))
    deriving (Data, Typeable)

instance Show IntSet where; show = show . fromIntSet
instance Eq IntSet where; a == b = fromIntSet a == fromIntSet b
instance Ord IntSet where; compare a b = compare (fromIntSet a) (fromIntSet b)

-- | Deconstruct a value of type 'IntSet'.
fromIntSet :: IntSet -> IntSet.IntSet
fromIntSet (IntSet (Invariant _ (_,Hide x))) = x

-- | Construct a value of type 'IntSet'.
toIntSet :: IntSet.IntSet -> IntSet
toIntSet x = IntSet $ Invariant inv $ create x
    where
        create x = (Trigger False $ IntSet.toList x, Hide x)

        inv (ks,x)
            | trigger ks = create $ IntSet.fromList $ fromTrigger ks
            | otherwise = (ks,x)
