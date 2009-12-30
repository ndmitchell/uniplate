{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}

{- |
    /RECOMMENDATION:/ Use "Data.Generics.Uniplate.Data" instead - it usually performs
    faster (sometimes significantly so) and requires no special instance declarations.

    This module supplies a method for writing 'Uniplate' / 'Biplate' instances. One
    instance declaration is required for each data type you wish to work with. The
    instances can be generated using Derive: <http://community.haskell.org/~ndm/derive/>.

    To take an example:

    > data Expr = Var Int | Neg Expr | Add Expr Expr
    >             deriving Typeable
    >
    > instance (Typeable a, Uniplate a) => PlateAll Expr a where
    >     plateAll (Var x  ) = plate Var |+ x
    >     plateAll (Neg x  ) = plate Neg |+ x
    >     plateAll (Add x y) = plate Add |+ x |+ y
-}

module Data.Generics.Uniplate.Typeable(
    module Data.Generics.Uniplate.Operations,
    module Data.Typeable,
    -- * The Class
    PlateAll(..),
    -- * The Combinators
    plate, (|+), (|-), plateProject
    ) where

import Control.Arrow
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Internal.Utils
import Data.Generics.Str
import Data.Typeable
import Data.Ratio


instance (Typeable a, Typeable b, Uniplate b, PlateAll a b) => Biplate a b where
    biplate = plateMore

instance PlateAll a a => Uniplate a where
    uniplate = plateAll


type Type from to = (Str to, Str to -> from)


plateMore :: (Typeable from, Typeable to, PlateAll from to) => from -> Type from to
plateMore x = res
    where
        res = case asTypeOf (cast x) (Just $ strType $ fst res) of
                  Nothing -> plateAll x
                  Just y -> (One y, \(One y) -> unsafeCoerce y)


-- | This class should be defined for each data type of interest.
class PlateAll from to where
    -- | This method should be defined using 'plate' and '|+', '|-'.
    plateAll :: from -> Type from to


-- | The main combinator used to start the chain.
plate :: from -> Type from to
plate x = (Zero, \_ -> x)


-- | The field to the right may contain the target.
(|+) :: (Typeable item, Typeable to, PlateAll item to) => Type (item -> from) to -> item -> Type from to
(|+) (xs,x_) y = case plateMore y of
                      (ys,y_) -> (Two xs ys,\(Two xs ys) -> x_ xs (y_ ys))

-- | The field to the right /does not/ contain the target.
--   This can be used as either an optimisation, or more commonly for excluding
--   primitives such as Int.
(|-) :: Type (item -> from) to -> item -> Type from to
(|-) (xs,x_) y = (xs,\xs -> x_ xs y)


-- | Write an instance in terms of a projection/injection pair. Usually used to define instances
--   for abstract containers such as Map:
--
-- > instance (Ord a, Typeable a, PlateAll a c, Typeable b, PlateAll b c,
-- >          Typeable c, PlateAll c c) => PlateAll (Map.Map a b) c where
-- >     plateAll = plateProject Map.toList Map.fromList
plateProject :: (Typeable item, Typeable to, PlateAll item to) => (from -> item) -> (item -> from) -> from -> Type from to
plateProject into outof = second (outof . ) . plateAll . into


-- * Instances

-- ** Primitive Types

instance PlateAll Int to where plateAll x = plate x
instance PlateAll Bool to where plateAll x = plate x
instance PlateAll Char to where plateAll x = plate x
instance PlateAll Integer to where plateAll x = plate x
instance PlateAll Double to where plateAll x = plate x
instance PlateAll Float to where plateAll x = plate x
instance PlateAll () to where plateAll x = plate x

-- ** Container Types

instance (PlateAll from to, Typeable from, Typeable to, Uniplate to) => PlateAll [from] to where
    plateAll []     = plate []
    plateAll (x:xs) = plate (:) |+ x |+ xs

instance (PlateAll from to, Typeable from, Typeable to, Uniplate to) => PlateAll (Maybe from) to where
    plateAll Nothing  = plate Nothing
    plateAll (Just x) = plate Just |+ x

instance (PlateAll a to, Typeable a, PlateAll b to, Typeable b, Typeable to, Uniplate to) =>
         PlateAll (Either a b) to where
    plateAll (Left  x) = plate Left  |+ x
    plateAll (Right x) = plate Right |+ x

instance (PlateAll a to, Typeable a
         ,PlateAll b to, Typeable b
         ,Typeable to, Uniplate to) =>
         PlateAll (a,b) to where
    plateAll (a,b) = plate (,) |+ a |+ b

instance (PlateAll a to, Typeable a
         ,PlateAll b to, Typeable b
         ,PlateAll c to, Typeable c
         ,Typeable to, Uniplate to) =>
         PlateAll (a,b,c) to where
    plateAll (a,b,c) = plate (,,) |+ a |+ b |+ c

instance (PlateAll a to, Typeable a
         ,PlateAll b to, Typeable b
         ,PlateAll c to, Typeable c
         ,PlateAll d to, Typeable d
         ,Typeable to, Uniplate to) =>
         PlateAll (a,b,c,d) to where
    plateAll (a,b,c,d) = plate (,,,) |+ a |+ b |+ c |+ d

instance (PlateAll a to, Typeable a
         ,PlateAll b to, Typeable b
         ,PlateAll c to, Typeable c
         ,PlateAll d to, Typeable d
         ,PlateAll e to, Typeable e
         ,Typeable to, Uniplate to) =>
         PlateAll (a,b,c,d,e) to where
    plateAll (a,b,c,d,e) = plate (,,,,) |+ a |+ b |+ c |+ d |+ e

instance (Integral a, PlateAll a to, Typeable a, Typeable to, Uniplate to) => PlateAll (Ratio a) to where
    plateAll = plateProject (\x -> (numerator x, denominator x)) (uncurry (%))
