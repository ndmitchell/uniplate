{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{- |
    This module supplies a method for writing 'Uniplate' and 'Biplate' instances.
    This moulde gives the highest performance, but requires many instance definitions. The
    instances can be generated using Derive: <http://community.haskell.org/~ndm/derive/>.
    
    To take an example:
    
    > data Expr = Var Int | Pos Expr String | Neg Expr | Add Expr Expr
    > data Stmt = Seq [Stmt] | Sel [Expr] | Let String Expr
    >
    > instance Uniplate Expr where
    >     uniplate (Var x  ) = plate Var |- x
    >     uniplate (Pos x y) = plate Pos |* x |- y
    >     uniplate (Neg x  ) = plate Neg |* x
    >     uniplate (Add x y) = plate Add |* x |* y
    >
    > instance Biplate Expr Expr where
    >     biplate = plateSelf
    >
    > instance Uniplate Stmt where
    >     uniplate (Seq x  ) = plate Seq ||* x
    >     uniplate (Sel x  ) = plate Sel ||+ x
    >     uniplate (Let x y) = plate Let |-  x |- y
    >
    > instance Biplate Stmt Stmt where
    >     biplate = plateSelf
    >
    > instance Biplate Stmt Expr where
    >     biplate (Seq x  ) = plate Seq ||+ x
    >     biplate (Sel x  ) = plate Sel ||* x
    >     biplate (Let x y) = plate Let |-  x |* y

    To define instances for abstract data types, such as @Map@ or @Set@ from the @containers@ package,
    use 'plateProject'.

    This module provides a few monomorphic instances of 'Uniplate' / 'Biplate'
    for common types available in the base library, but does not provide any polymorphic
    instances. Given only monomorphic instances it is trivial to ensure that all instances
    are disjoint, making it easier to add your own instances.

    When defining polymorphic instances, be carefully to mention all potential children.
    Consider @Biplate Int (Int, a)@ - this instance cannot be correct because it will fail
    to return both @Int@ values on @(Int,Int)@. There are some legitimate polymorphic instances,
    such as @Biplate a [a]@ and @Biplate a a@, but take care to avoid overlapping instances.
-}
module Data.Generics.Uniplate.Direct(
    module Data.Generics.Uniplate.Operations,
    -- * The Combinators
    plate, plateSelf,
    (|+), (|-), (|*), (||+), (||*),
    plateProject
    ) where

import Control.Applicative
import Data.Generics.Uniplate.Operations
import Data.Ratio
import Data.Traversable


type Type m from to = (to -> m to) -> m from

-- | The main combinator used to start the chain.
--
-- The following rule can be used for optimisation:
--
-- > plate Ctor |- x == plate (Ctor x)
{-# INLINE[1] plate #-}
plate :: Applicative m => from -> Type m from to
plate f op = pure f


{-# RULES
"plate/-"    forall f x. plate f |- x = plate (f x)
"plate/+"    forall f x. plate f |+ x = platePlus f x
"plate/*"    forall f x. plate f |* x = plateStar f x #-}


{-# INLINE plateStar #-}
plateStar :: Applicative m => (to -> from) -> to -> Type m from to
plateStar f x op = f <$> op x

{-# INLINE platePlus #-}
platePlus :: (Applicative m, Biplate item to) => (item -> from) -> item -> Type m from to
platePlus f x op = f <$> descendBiM op x

-- | The field to the right is the target.
{-# INLINE[1] (|*) #-}
(|*) :: Applicative m => Type m (to -> from) to -> to -> Type m from to
(|*) f x op = f op <*> op x

-- | The field to the right may contain the target.
{-# INLINE[1] (|+) #-}
(|+) :: (Applicative m, Biplate item to) => Type m (item -> from) to -> item -> Type m from to
(|+) f x op = f op <*> descendBiM op x

-- | The field to the right /does not/ contain the target.
{-# INLINE[1] (|-) #-}
(|-) :: Applicative m => Type m (item -> from) to -> item -> Type m from to
(|-) f x op = f op <*> pure x


-- | The field to the right is a list of the type of the target
{-# INLINE (||*) #-}
(||*) :: Applicative m => Type m ([to] -> from) to -> [to] -> Type m from to
(||*) f x op = f op <*> traverse op x


-- | The field to the right is a list of types which may contain the target
(||+) :: (Applicative m, Biplate item to) => Type m ([item] -> from) to -> [item] -> Type m from to
(||+) f x op = f op <*> traverse (descendBiM op) x


-- | Used for 'Biplate' definitions where both types are the same.
plateSelf :: Applicative m => to -> Type m to to
plateSelf x f = f x


-- | Write an instance in terms of a projection/injection pair. Usually used to define instances
--   for abstract containers such as Map:
--
-- > instance Biplate (Map.Map [Char] Int) Char where
-- >     biplate = plateProject Map.toList Map.fromList
--
--   If the types ensure that no operations will not change the keys
--   we can use the 'fromDistictAscList' function to reconstruct the Map:
--
-- > instance Biplate (Map.Map [Char] Int) Int where
-- >     biplate = plateProject Map.toAscList Map.fromDistinctAscList
plateProject :: (Applicative m, Biplate item to) => (from -> item) -> (item -> from) -> from -> Type m from to
plateProject into outof x op = outof <$> descendBiM op (into x)


instance Uniplate Int where uniplate x = plate x
instance Uniplate Bool where uniplate x = plate x
instance Uniplate Char where uniplate x = plate x
instance Uniplate Integer where uniplate x = plate x
instance Uniplate Double where uniplate x = plate x
instance Uniplate Float where uniplate x = plate x
instance Uniplate () where uniplate x = plate x

instance Uniplate [Char] where
    uniplate (x:xs) = plate (x:) |* xs
    uniplate x = plate x

instance Biplate [Char] Char where
    biplate (x:xs) = plate (:) |* x ||* xs
    biplate x = plate x

instance Biplate [Char] [Char] where
    biplate = plateSelf

instance Uniplate (Ratio Integer) where
    uniplate = plate

instance Biplate (Ratio Integer) (Ratio Integer) where
    biplate = plateSelf

instance Biplate (Ratio Integer) Integer where
    biplate x f = (%) <$> f (numerator x) <*> f (denominator x)
