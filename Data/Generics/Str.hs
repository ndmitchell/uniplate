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

strList x = builder (f x)
    where
        f (Two (One x) xs) cons nil = x `cons` f xs cons nil
        f Zero cons nil = nil

listStr (x:xs) = Two (One x) (listStr xs)
listStr [] = Zero
