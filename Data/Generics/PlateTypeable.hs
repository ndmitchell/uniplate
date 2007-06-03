{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}

{- |
    This module supplies a method for writing 'Biplate' instances more easily.
    
    To take an example:
    
    > data Expr = Var Int | Neg Expr | Add Expr Expr
    >
    > instance Typeable Expr where ...
    >
    > instance (Typeable a, Uniplate a) => PlateAll Expr a where
    >   plateAll (Var x  ) = plate Var |- x
    >   plateAll (Neg x  ) = plate Neg |+ x
    >   plateAll (Add x y) = plate Add |+ x |+ y
    >
    > instance Uniplate Expr where
    >   replaceChildren = replaceChildrenAll
-}

module Data.Generics.PlateTypeable(
    module Data.Generics.Biplate,
    module Data.Typeable,
    -- * The Class
    PlateAll(..), replaceChildrenAll,
    -- * The Combinators
    plate, (|+), (|-)
    ) where

import Data.Generics.Biplate
import Data.Generics.PlateInternal
import Data.Typeable
import Data.Maybe


instance (Typeable a, Typeable b, Uniplate b, PlateAll a b) => Biplate a b where
    replaceType x = liftType $ plateMore x


-- | This function is used to write a 'Uniplate' instance from a 'PlateAll' one
replaceChildrenAll :: PlateAll a b => a -> ([b],[b] -> a)
replaceChildrenAll a = liftType $ plateAll a



type Type from to = ([to] -> [to], [to] -> (from,[to]))


liftType :: Type from to -> ([to], [to] -> from)
liftType (a,b) = (a [], fst . b)


plateMore :: (Typeable from, Typeable to, PlateAll from to) => from -> Type from to
plateMore x = res
    where
        res = case asTypeOf (cast x) (Just $ head $ fst res []) of
                  Nothing -> plateAll x
                  Just y -> ((y:), \(y:ys) -> (unsafeCast y, ys))


-- | This class represents going from the container type to the target.
--
-- This class should only be constructed with 'plate', '|+' and '|-'
class PlateAll from to where
    plateAll :: from -> Type from to


-- | The main combinator used to start the chain.
--
-- The following rule can be used for optimisation:
--
-- > plate Ctor |- x == plate (Ctor x)
plate :: from -> Type from to
plate f = (id, \xs -> (f,xs))


-- | the field to the right may contain the target.
(|+) :: (Typeable item, Typeable to, PlateAll item to) => Type (item -> from) to -> item -> Type from to
(|+) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        (collectR,generateR) = plateMore item
        collect2 = collectL . collectR
        generate2 xs = case generateL xs of
                        (a,xs) -> case generateR xs of
                         (b,xs) -> (a b, xs)

-- | The field to the right /does not/ contain the target.
-- This can be used as either an optimisation, or more commonly for excluding
-- primitives such as Int.
(|-) :: Type (item -> from) to -> item -> Type from to
(|-) (collect,generate) item = (collect,\xs -> case generate xs of (r,xs) -> (r item, xs))


-- * Instances

instance (PlateAll from to, Typeable from, Typeable to, Uniplate to) => PlateAll [from] to where
    plateAll x = case x of
        [] -> plate []
        (x:xs) -> plate (:) |+ x |+ xs
