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

    This module provides a few monomorphic instances of 'Uniplate'/'Biplate'
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

import Control.Arrow
import Data.Generics.Uniplate.Operations
import Data.Generics.Str
import Data.Ratio


type Type from to = (Str to, Str to -> from)

-- | The main combinator used to start the chain.
--
-- The following rule can be used for optimisation:
--
-- > plate Ctor |- x == plate (Ctor x)
{-# INLINE plate #-}
plate :: from -> Type from to
plate f = (Zero, \_ -> f)


-- | The field to the right is the target.
{-# INLINE (|*) #-}
(|*) :: Type (to -> from) to -> to -> Type from to
(|*) (xs,x_) y = (Two xs (One y),\(Two xs (One y)) -> x_ xs y)



-- | The field to the right may contain the target.
{-# INLINE (|+) #-}
(|+) :: Biplate item to => Type (item -> from) to -> item -> Type from to
(|+) (xs,x_) y = case biplate y of
                      (ys,y_) -> (Two xs ys, \(Two xs ys) -> x_ xs (y_ ys))


-- | The field to the right /does not/ contain the target.
{-# INLINE (|-) #-}
(|-) :: Type (item -> from) to -> item -> Type from to
(|-) (xs,x_) y = (xs,\xs -> x_ xs y)


-- | The field to the right is a list of the type of the target
{-# INLINE (||*) #-}
(||*) :: Type ([to] -> from) to -> [to] -> Type from to
(||*) (xs,x_) y = (Two xs (listStr y), \(Two xs ys) -> x_ xs (strList ys))


-- | The field to the right is a list of types which may contain the target
(||+) :: Biplate item to => Type ([item] -> from) to -> [item] -> Type from to
(||+) (xs,x_) [] = (xs, \xs -> x_ xs []) -- can eliminate a Two _ Zero in the base case
(||+) (xs,x_) (y:ys) = case plate (:) |+ y ||+ ys of
                       (ys,y_) -> (Two xs ys, \(Two xs ys) -> x_ xs (y_ ys))


-- | Used for 'Biplate' definitions where both types are the same.
plateSelf :: to -> Type to to
plateSelf x = (One x, \(One x) -> x)


-- | Write an instance in terms of a projection/injection pair. Usually used to define instances
--   for abstract containers such as Map:
--
-- > instance Biplate (Map.Map [Char] Int) Int where
-- >     biplate = plateProject Map.toAscList Map.fromDistinctAscList
--
-- Because we know that Uniplate operations will not change the keys,
-- we can use the 'fromDistictAscList' function to reconstruct it.
plateProject :: Biplate item to => (from -> item) -> (item -> from) -> from -> Type from to
plateProject into outof = second (outof . ) . biplate . into


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
    biplate x = (Two (One (numerator x)) (One (denominator x)), \(Two (One n) (One d)) -> n % d)
