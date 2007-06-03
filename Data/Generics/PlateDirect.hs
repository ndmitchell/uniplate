{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}

{- |
    This module supplies a method for writing 'Biplate' instances more easily.
    This module requires fewest extensions, highest performance, and most instance
    definitions.
    
    To take an example:
    
    > data Expr = Var Int | Pos Expr String | Neg Expr | Add Expr Expr
    > data Stmt = Seq [Stmt] | Sel [Expr] | Let String Expr
    >
    > instance PlateOne Expr where
    >     plateOne (Var x  ) = plate Var |- x
    >     plateOne (Pos x y) = plate Pos |* x |- y
    >     plateOne (Neg x  ) = plate Neg |* x
    >     plateOne (Add x y) = plate Add |* x |* y
    >
    > instance PlateAll Expr Expr where
    >     plateAll = plateSelf
    >
    > instance PlateOne Stmt where
    >     plateOne (Seq x  ) = plate Seq ||* x
    >     plateOne (Sel x  ) = plate Sel ||+ x
    >     plateOne (Let x y) = plate Let |-  x |- y
    >
    > instance PlateAll Stmt Stmt where
    >     plateAll = plateSelf
    >
    > instance PlateAll Stmt Expr where
    >     plateAll (Seq x  ) = plate Seq ||+ x
    >     plateAll (Sel x  ) = plate Sel ||* x
    >     plateAll (Let x y) = plate Let |-  x |* y
-}
    

module Data.Generics.PlateDirect(
    module Data.Generics.Biplate,
    -- * The Classes
    PlateAll(..), PlateOne(..),
    -- * The Combinators
    plate, plateSelf,
    (|+), (|-), (|*), (||+), (||*)
    ) where

import Data.Generics.Biplate
import Data.Generics.PlateInternal
import Data.Maybe


instance (Uniplate b, PlateAll a b) => Biplate a b where
    replaceType x = liftType $ plateAll x

instance PlateOne a => Uniplate a where
    replaceChildren x = liftType $ plateOne x


type Type from to = ([to] -> [to], [to] -> (from,[to]))


liftType :: Type from to -> ([to], [to] -> from)
liftType (a,b) = (a [], fst . b)


-- | This class represents going from the container type to the target.
--
-- If @from == to@ then use 'plateSelf', otherwise use 'plate' and the
-- other combinators.
class PlateAll from to where
    plateAll :: from -> Type from to

-- | This class is for when the target and container are the same type.
class PlateOne to where
    plateOne :: to -> Type to to


-- | The main combinator used to start the chain.
--
-- The following rule can be used for optimisation:
--
-- > plate Ctor |- x == plate (Ctor x)
plate :: from -> Type from to
plate f = (id, \xs -> (f,xs))


-- | The field to the right is the target.
(|*) :: Type (to -> from) to -> to -> Type from to
(|*) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        collect2 = collectL . (item:)
        generate2 xs = case generateL xs of
                        (a,(b:xs)) -> (a b, xs)


-- | the field to the right may contain the target.
(|+) :: PlateAll item to => Type (item -> from) to -> item -> Type from to
(|+) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        (collectR,generateR) = plateAll item
        collect2 = collectL . collectR
        generate2 xs = case generateL xs of
                        (a,xs) -> case generateR xs of
                         (b,xs) -> (a b, xs)


-- | The field to the right /does not/ contain the target.
(|-) :: Type (item -> from) to -> item -> Type from to
(|-) (collect,generate) item = (collect,\xs -> case generate xs of (r,xs) -> (r item, xs))


-- | The field to the right is a list of the type of the target
(||*) :: Type ([to] -> from) to -> [to] -> Type from to
(||*) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        collect2 = collectL . (item++)
        generate2 xs = case generateL xs of
                        (a,xs) -> let (x1,x2) = splitAt (length item) xs
                                  in (a x1,x2)


-- | The field to the right is a list of types which may contain the target
(||+) :: PlateAll item to => Type ([item] -> from) to -> [item] -> Type from to
(||+) f item = (collect2,generate2)
    where
        (collectL,generateL) = f
        (collectR,generateR) = plateListDiff item
        collect2 = collectL . collectR
        generate2 xs = case generateL xs of
                        (a,xs) -> case generateR xs of
                         (b,xs) -> (a b, xs)

plateListDiff [] = plate []
plateListDiff (x:xs) = plate (:) |+ x ||+ xs


-- | Used for 'PlayAll' definitions where both types are the same.
plateSelf :: to -> Type to to
plateSelf x = ((x:), \(x:xs) -> (x,xs))
