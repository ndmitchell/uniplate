{-# OPTIONS_GHC -fglasgow-exts #-}

module DeriveTypeable where

import Data
import Data.Typeable
import Data.Generics.PlateTypeable


instance Uniplate NExpr where
    uniplate = uniplateAll

instance (Typeable a, Uniplate a) => PlateAll NExpr a where
    plateAll x =
        case x of
            NNeg a    -> plate NNeg |+ a
            NAdd a b  -> plate NAdd |+ a |+ b
            NSub a b  -> plate NSub |+ a |+ b
            NMul a b  -> plate NMul |+ a |+ b
            NDiv a b  -> plate NDiv |+ a |+ b
            _ -> plate x


instance Uniplate NStm where
    uniplate = uniplateAll

instance Uniplate NExp where
    uniplate = uniplateAll

instance Uniplate NVar where
    uniplate = uniplateAll

instance Uniplate NTyp where
    uniplate = uniplateAll


instance (Typeable a, Uniplate a) => PlateAll NStm a where
    plateAll x =
        case x of
            NSDecl a b -> plate NSDecl   |+ a |+ b
            NSAss  a b -> plate NSAss    |+ a |+ b
            NSBlock a  -> plate NSBlock  |+ a
            NSReturn a -> plate NSReturn |+ a

instance (Typeable a, Uniplate a) => PlateAll NExp a where
    plateAll x =
        case x of
            NEStm a   -> plate NEStm |+ a
            NEAdd a b -> plate NEAdd |+ a |+ b
            NEVar a   -> plate NEVar |+ a
            _ -> plate x

instance (Typeable a, Uniplate a) => PlateAll NVar a where
    plateAll x = plate x

instance (Typeable a, Uniplate a) => PlateAll NTyp a where
    plateAll x = plate x



instance Uniplate NCompany where uniplate = uniplateAll
instance Uniplate NDept where uniplate = uniplateAll
instance Uniplate NUnt where uniplate = uniplateAll
instance Uniplate NEmployee where uniplate = uniplateAll
instance Uniplate NPerson where uniplate = uniplateAll
instance Uniplate NSalary where uniplate = uniplateAll

instance (Typeable a, Uniplate a) => PlateAll NCompany a where
    plateAll (NC x) = plate NC |+ x

instance (Typeable a, Uniplate a) => PlateAll NDept a where
    plateAll (ND x y z) = plate (ND x) |+ y |+ z

instance (Typeable a, Uniplate a) => PlateAll NUnt a where
    plateAll (NPU x) = plate NPU |+ x
    plateAll (NDU x) = plate NDU |+ x

instance (Typeable a, Uniplate a) => PlateAll NEmployee a where
    plateAll (NE x y) = plate NE |+ x |+ y

instance (Typeable a, Uniplate a) => PlateAll NPerson a where
    plateAll x = plate x

instance (Typeable a, Uniplate a) => PlateAll NSalary a where
    plateAll x = plate x
