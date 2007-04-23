{-# OPTIONS_GHC -fglasgow-exts #-}

module DeriveManual where

import Data
import Data.Generics.PlayManual


instance PlayOne NExpr where
    playOne x =
        case x of
            NNeg  a    -> play NNeg |* a
            NAdd  a b  -> play NAdd |* a |* b
            NSub  a b  -> play NSub |* a |* b
            NMul  a b  -> play NMul |* a |* b
            NDiv  a b  -> play NDiv |* a |* b
            _          -> play x


instance PlayAll NStm NVar where
    playAll x =
        case x of
            NSDecl a b -> play (NSDecl a) |* b
            NSAss a b -> play NSAss |* a |+ b
            NSBlock x -> play NSBlock ||+ x
            NSReturn x -> play NSReturn |+ x

instance PlayAll NStm NStm where
    playAll = playSelf

instance PlayOne NVar where
    playOne x = play x

instance PlayAll NExp NVar where
    playAll x =
        case x of
            NEStm x -> play NEStm |+ x
            NEAdd x y -> play NEAdd |+ x |+ y
            NEVar x -> play NEVar |* x
            _ -> play x

instance PlayOne NStm where
    playOne x =
        case x of
            NSAss x y -> play (NSAss x) |+ y
            NSBlock x -> play NSBlock ||* x
            NSReturn x -> play NSReturn |+ x
            _ -> play x

instance PlayAll NStm NExp where
    playAll x =
        case x of
            NSAss x y -> play (NSAss x) |* y
            NSBlock x -> play NSBlock ||+ x
            NSReturn x -> play NSReturn |* x
            _ -> play x

instance PlayAll NExp NStm where
    playAll x =
        case x of
            NEStm x -> play NEStm |* x
            NEAdd x y -> play NEAdd |+ x |+ y
            _ -> play x

instance PlayOne NExp where
    playOne x =
        case x of
            NEStm x -> play NEStm |+ x
            NEAdd x y -> play NEAdd |* x |* y
            _ -> play x


{-
data NCompany = NC [NDept] deriving (Data,Typeable)
data NDept = ND String NEmployee [NUnt] deriving (Data,Typeable)
data NUnt = NPU NEmployee | NDU NDept deriving (Data,Typeable)
data NEmployee = NE NPerson NSalary deriving (Data,Typeable)
data NPerson = NP String String deriving (Data,Typeable)
data NSalary = NS Integer deriving (Data,Typeable)
-}

instance PlayAll NCompany NSalary where
    playAll (NC x) = play NC ||+ x

instance PlayAll NDept NSalary where
    playAll (ND a b c) = play (ND a) |+ b ||+ c

instance PlayAll NUnt NSalary where
    playAll (NPU a) = play NPU |+ a
    playAll (NDU a) = play NDU |+ a

instance PlayAll NUnt NDept where
    playAll (NDU a) = play NDU |* a
    playAll x = play x

instance PlayAll NEmployee NSalary where
    playAll (NE a b) = play (NE a) |* b

instance PlayOne NSalary where
    playOne x = play x

instance PlayOne NDept where
    playOne (ND a b c) = play (ND a b) ||+ c

instance PlayAll NCompany NDept where
    playAll (NC x) = play NC ||* x
