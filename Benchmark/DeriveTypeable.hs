{-# OPTIONS_GHC -fglasgow-exts #-}

module DeriveTypeable where

import Data
import Data.Typeable
import Data.Generics.PlayTypeable


instance Play NExpr where
    replaceChildren = replaceChildrenAll

instance (Typeable a, Play a) => PlayAll NExpr a where
    playAll x =
        case x of
            NNeg a    -> play NNeg |+ a
            NAdd a b  -> play NAdd |+ a |+ b
            NSub a b  -> play NSub |+ a |+ b
            NMul a b  -> play NMul |+ a |+ b
            NDiv a b  -> play NDiv |+ a |+ b
            _ -> play x


instance Play NStm where
    replaceChildren = replaceChildrenAll

instance Play NExp where
    replaceChildren = replaceChildrenAll

instance Play NVar where
    replaceChildren = replaceChildrenAll

instance Play NTyp where
    replaceChildren = replaceChildrenAll


instance (Typeable a, Play a) => PlayAll NStm a where
    playAll x =
        case x of
            NSDecl a b -> play NSDecl   |+ a |+ b
            NSAss  a b -> play NSAss    |+ a |+ b
            NSBlock a  -> play NSBlock  |+ a
            NSReturn a -> play NSReturn |+ a

instance (Typeable a, Play a) => PlayAll NExp a where
    playAll x =
        case x of
            NEStm a   -> play NEStm |+ a
            NEAdd a b -> play NEAdd |+ a |+ b
            NEVar a   -> play NEVar |+ a
            _ -> play x

instance (Typeable a, Play a) => PlayAll NVar a where
    playAll x = play x

instance (Typeable a, Play a) => PlayAll NTyp a where
    playAll x = play x



instance Play NCompany where replaceChildren = replaceChildrenAll
instance Play NDept where replaceChildren = replaceChildrenAll
instance Play NUnt where replaceChildren = replaceChildrenAll
instance Play NEmployee where replaceChildren = replaceChildrenAll
instance Play NPerson where replaceChildren = replaceChildrenAll
instance Play NSalary where replaceChildren = replaceChildrenAll

instance (Typeable a, Play a) => PlayAll NCompany a where
    playAll (NC x) = play NC |+ x

instance (Typeable a, Play a) => PlayAll NDept a where
    playAll (ND x y z) = play (ND x) |+ y |+ z

instance (Typeable a, Play a) => PlayAll NUnt a where
    playAll (NPU x) = play NPU |+ x
    playAll (NDU x) = play NDU |+ x

instance (Typeable a, Play a) => PlayAll NEmployee a where
    playAll (NE x y) = play NE |+ x |+ y

instance (Typeable a, Play a) => PlayAll NPerson a where
    playAll x = play x

instance (Typeable a, Play a) => PlayAll NSalary a where
    playAll x = play x
