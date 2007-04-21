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
