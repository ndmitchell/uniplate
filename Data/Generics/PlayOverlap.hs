{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances -fallow-incoherent-instances #-}

-- this module should not be imported directly

module Data.Generics.PlayOverlap(module Data.Generics.PlayEx, module Data.Generics.PlayOverlap) where

import Data.Generics.PlayEx


data HTrue
data HFalse

class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x

class  TypeEq x y b | x y -> b
instance TypeEq x x HTrue
instance TypeCast HFalse b => TypeEq x y b


instance (TypeEq a a HTrue, Play a) => PlayEx a a where
    replaceType = playSelf

instance (TypeEq a b HFalse, PlayAll a b, Play b) => PlayEx a b where
    replaceType = replaceAll
