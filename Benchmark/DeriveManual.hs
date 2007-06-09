{-# OPTIONS_GHC -fglasgow-exts #-}

module DeriveManual where

import Data
import Data.Generics.PlateDirect


instance PlateOne NExpr where
    plateOne x =
        case x of
            NNeg  a    -> plate NNeg |* a
            NAdd  a b  -> plate NAdd |* a |* b
            NSub  a b  -> plate NSub |* a |* b
            NMul  a b  -> plate NMul |* a |* b
            NDiv  a b  -> plate NDiv |* a |* b
            _          -> plate x


instance PlateAll NStm NVar where
    plateAll x =
        case x of
            NSDecl a b -> plate (NSDecl a) |* b
            NSAss a b -> plate NSAss |* a |+ b
            NSBlock x -> plate NSBlock ||+ x
            NSReturn x -> plate NSReturn |+ x

instance PlateAll NStm NStm where
    plateAll = plateSelf

instance PlateOne NVar where
    plateOne x = plate x

instance PlateAll NExp NVar where
    plateAll x =
        case x of
            NEStm x -> plate NEStm |+ x
            NEAdd x y -> plate NEAdd |+ x |+ y
            NEVar x -> plate NEVar |* x
            _ -> plate x

instance PlateOne NStm where
    plateOne x =
        case x of
            NSAss x y -> plate (NSAss x) |+ y
            NSBlock x -> plate NSBlock ||* x
            NSReturn x -> plate NSReturn |+ x
            _ -> plate x

instance PlateAll NStm NExp where
    plateAll x =
        case x of
            NSAss x y -> plate (NSAss x) |* y
            NSBlock x -> plate NSBlock ||+ x
            NSReturn x -> plate NSReturn |* x
            _ -> plate x

instance PlateAll NExp NStm where
    plateAll x =
        case x of
            NEStm x -> plate NEStm |* x
            NEAdd x y -> plate NEAdd |+ x |+ y
            _ -> plate x

instance PlateOne NExp where
    plateOne x =
        case x of
            NEStm x -> plate NEStm |+ x
            NEAdd x y -> plate NEAdd |* x |* y
            _ -> plate x


{-
data NCompany = NC [NDept] deriving (Data,Typeable)
data NDept = ND String NEmployee [NUnt] deriving (Data,Typeable)
data NUnt = NPU NEmployee | NDU NDept deriving (Data,Typeable)
data NEmployee = NE NPerson NSalary deriving (Data,Typeable)
data NPerson = NP String String deriving (Data,Typeable)
data NSalary = NS Integer deriving (Data,Typeable)
-}

instance PlateAll NCompany NSalary where
    plateAll (NC x) = plate NC ||+ x

instance PlateAll NDept NSalary where
    plateAll (ND a b c) = plate (ND a) |+ b ||+ c

instance PlateAll NUnt NSalary where
    plateAll (NPU a) = plate NPU |+ a
    plateAll (NDU a) = plate NDU |+ a

instance PlateAll NUnt NDept where
    plateAll (NDU a) = plate NDU |* a
    plateAll x = plate x

instance PlateAll NEmployee NSalary where
    plateAll (NE a b) = plate (NE a) |* b

instance PlateOne NSalary where
    plateOne x = plate x

instance PlateOne NDept where
    plateOne (ND a b c) = plate (ND a b) ||+ c

instance PlateAll NCompany NDept where
    plateAll (NC x) = plate NC ||* x
