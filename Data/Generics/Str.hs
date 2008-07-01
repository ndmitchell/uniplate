{- |
This is the main central data structure in the Uniplate system.
-}

module Data.Generics.Str where

import Data.Generics.PlateInternal

import Data.Traversable
import Data.Foldable
import Control.Applicative
import Data.Monoid

-- * The Data Type

data Str a = Zero | One a | Two (Str a) (Str a)
             deriving Show


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


strType :: Str a -> a
strType = undefined


strList :: Str a -> [a]
strList x = builder (f x)
    where
        f (Two (One x) xs) cons nil = x `cons` f xs cons nil
        f Zero cons nil = nil


listStr :: [a] -> Str a
listStr (x:xs) = Two (One x) (listStr xs)
listStr [] = Zero


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
