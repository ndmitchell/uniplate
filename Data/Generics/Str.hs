{-# LANGUAGE BangPatterns #-}
{- |
    This module provides the 'Str' data type, which is used by the
    underlying 'uniplate' and 'biplate' methods. It should not
    be used directly under normal circumstances.
-}

module Data.Generics.Str where

import Data.Generics.Uniplate.Internal.Utils

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Traversable


-- * The Data Type

data Str a = Zero | One a | Two (Str a) (Str a)
             deriving Show

instance Eq a => Eq (Str a) where
    Zero == Zero = True
    One x == One y = x == y
    Two x1 x2 == Two y1 y2 = x1 == y1 && x2 == y2
    _ == _ = False


{-# INLINE strMap #-}
strMap :: (a -> b) -> Str a -> Str b
strMap f x = g SPEC x
    where
        g !spec Zero = Zero
        g !spec (One x) = One $ f x
        g !spec (Two x y) = Two (g spec x) (g spec y)



{-# INLINE strMapM #-}
strMapM :: Applicative m => (a -> m b) -> Str a -> m (Str b)
strMapM f x = g SPEC x
    where
        g !spec Zero = pure Zero
        g !spec (One x) = One <$> f x
        g !spec (Two x y) = Two <$> g spec x <*> g spec y


instance Functor Str where
    fmap f Zero = Zero
    fmap f (One x) = One (f x)
    fmap f (Two x y) = Two (fmap f x) (fmap f y)



instance Foldable Str where
    foldMap m Zero = mempty
    foldMap m (One x) = m x
    foldMap m (Two l r) = foldMap m l `mappend` foldMap m r


instance Traversable Str where
    traverse f Zero = pure Zero
    traverse f (One x) = One <$> f x
    traverse f (Two x y) = Two <$> traverse f x <*> traverse f y


-- | Take the type of the method, will crash if called
strType :: Str a -> a
strType = error "Data.Generics.Str.strType: Cannot be called"


-- | Convert a 'Str' to a list, assumes the value was created
--   with 'listStr'
strList :: Str a -> [a]
strList x = builder (f x)
    where
        f (Two (One x) xs) cons nil = x `cons` f xs cons nil
        f Zero cons nil = nil


-- | Convert a list to a 'Str'
listStr :: [a] -> Str a
listStr (x:xs) = Two (One x) (listStr xs)
listStr [] = Zero


-- | Transform a 'Str' to a list, and back again, in a structure
--   preserving way. The output and input lists must be equal in
--   length.
strStructure :: Str a -> ([a], [a] -> Str a)
strStructure x = (g x [], fst . f x)
    where
        g :: Str a -> [a] -> [a]
        g Zero xs = xs
        g (One x) xs = x:xs
        g (Two a b) xs = g a (g b xs)

        f :: Str a -> [a] -> (Str a, [a])
        f Zero rs = (Zero, rs)
        f (One _) (r:rs) = (One r, rs)
        f (Two a b) rs1 = (Two a2 b2, rs3)
            where
                (a2,rs2) = f a rs1
                (b2,rs3) = f b rs2
