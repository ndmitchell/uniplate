{-# LANGUAGE MultiParamTypeClasses #-}

{- |
    /RECOMMENDATION/: Use "Data.Generics.Uniplate.Direct" instead.

    This module supplies a method for writing 'Biplate' instances more easily.
    This module requires fewest extensions, highest performance, and most instance
    definitions.
    
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
-}
    

module Data.Generics.PlateDirect(
    module Data.Generics.Biplate,
    -- * The Combinators
    plate, plateSelf,
    (|+), (|-), (|*), (||+), (||*)
    ) where

import Data.Generics.Biplate
import Data.Generics.PlateInternal
import Data.Generics.Str
import Data.Maybe


type Type from to = (Str to, Str to -> from)

-- | The main combinator used to start the chain.
--
-- The following rule can be used for optimisation:
--
-- > plate Ctor |- x == plate (Ctor x)
plate :: from -> Type from to
plate f = (Zero, \_ -> f)


-- | The field to the right is the target.
(|*) :: Type (to -> from) to -> to -> Type from to
(|*) (xs,x_) y = (Two xs (One y),\(Two xs (One y)) -> x_ xs y)



-- | The field to the right may contain the target.
(|+) :: Biplate item to => Type (item -> from) to -> item -> Type from to
(|+) (xs,x_) y = case biplate y of
                      (ys,y_) -> (Two xs ys, \(Two xs ys) -> x_ xs (y_ ys))


-- | The field to the right /does not/ contain the target.
(|-) :: Type (item -> from) to -> item -> Type from to
(|-) (xs,x_) y = (xs,\xs -> x_ xs y)


-- | The field to the right is a list of the type of the target
(||*) :: Type ([to] -> from) to -> [to] -> Type from to
(||*) (xs,x_) y = (Two xs (listStr y), \(Two xs ys) -> x_ xs (strList ys))


-- | The field to the right is a list of types which may contain the target
(||+) :: Biplate item to => Type ([item] -> from) to -> [item] -> Type from to
(||+) (xs,x_) y = case plateListDiff y of
                       (ys,y_) -> (Two xs ys, \(Two xs ys) -> x_ xs (y_ ys))
    where
        plateListDiff [] = plate []
        plateListDiff (x:xs) = plate (:) |+ x ||+ xs


-- | Used for 'PlayAll' definitions where both types are the same.
plateSelf :: to -> Type to to
plateSelf x = (One x, \(One x) -> x)
