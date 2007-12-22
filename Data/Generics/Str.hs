{- |
This is the main central data structure in the Uniplate system.
-}

module Data.Generics.Str where

import Data.Generics.PlateInternal

-- * The Data Type

data Str a = Zero | One a | Two (Str a) (Str a)
             deriving Show

instance Functor Str where
  fmap f Zero = Zero
  fmap f (One x) = One (f x)
  fmap f (Two x y) = Two (fmap f x) (fmap f y)

strType :: Str a -> a
strType = undefined

strList x = builder (f x)
    where
        f (Two (One x) xs) cons nil = x `cons` f xs cons nil
        f Zero cons nil = nil

listStr (x:xs) = Two (One x) (listStr xs)
listStr [] = Zero



pam f Zero = Zero
pam f (One x) = One (f x)
pam f (Two x y) = Two (pam f x) (pam f y)

pamM f Zero = return Zero
pamM f (One x) = do x <- f x; return (One x)
pamM f (Two x y) = do x <- pamM f x; y <- pamM f y; return (Two x y)
