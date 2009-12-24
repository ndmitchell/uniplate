{-# OPTIONS_GHC -fglasgow-exts #-}

module DeriveManual where

import Data
import Data.Generics.PlateDirect


instance Uniplate NExpr where
    uniplate x =
        case x of
            NNeg  a    -> plate NNeg |* a
            NAdd  a b  -> plate NAdd |* a |* b
            NSub  a b  -> plate NSub |* a |* b
            NMul  a b  -> plate NMul |* a |* b
            NDiv  a b  -> plate NDiv |* a |* b
            _          -> plate x


instance Biplate NStm NVar where
    biplate x =
        case x of
            NSDecl a b -> plate (NSDecl a) |* b
            NSAss a b -> plate NSAss |* a |+ b
            NSBlock x -> plate NSBlock ||+ x
            NSReturn x -> plate NSReturn |+ x

instance Biplate NStm NStm where
    biplate = plateSelf

instance Uniplate NVar where
    uniplate x = plate x

instance Biplate NExp NVar where
    biplate x =
        case x of
            NEStm x -> plate NEStm |+ x
            NEAdd x y -> plate NEAdd |+ x |+ y
            NEVar x -> plate NEVar |* x
            _ -> plate x

instance Uniplate NStm where
    uniplate x =
        case x of
            NSAss x y -> plate (NSAss x) |+ y
            NSBlock x -> plate NSBlock ||* x
            NSReturn x -> plate NSReturn |+ x
            _ -> plate x

instance Biplate NStm NExp where
    biplate x =
        case x of
            NSAss x y -> plate (NSAss x) |* y
            NSBlock x -> plate NSBlock ||+ x
            NSReturn x -> plate NSReturn |* x
            _ -> plate x

instance Biplate NExp NStm where
    biplate x =
        case x of
            NEStm x -> plate NEStm |* x
            NEAdd x y -> plate NEAdd |+ x |+ y
            _ -> plate x

instance Uniplate NExp where
    uniplate x =
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

instance Biplate NCompany NSalary where
    biplate (NC x) = plate NC ||+ x

instance Biplate NDept NSalary where
    biplate (ND a b c) = plate (ND a) |+ b ||+ c

instance Biplate NUnt NSalary where
    biplate (NPU a) = plate NPU |+ a
    biplate (NDU a) = plate NDU |+ a

instance Biplate NUnt NDept where
    biplate (NDU a) = plate NDU |* a
    biplate x = plate x

instance Biplate NEmployee NSalary where
    biplate (NE a b) = plate (NE a) |* b

instance Uniplate NSalary where
    uniplate x = plate x

instance Uniplate NDept where
    uniplate (ND a b c) = plate (ND a b) ||+ c

instance Biplate NCompany NDept where
    biplate (NC x) = plate NC ||* x
