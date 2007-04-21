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
            NVal a    -> play NVal |- a
            NVar a    -> play NVar |- a
            NNeg a    -> play NNeg |+ a
            NAdd a b  -> play NAdd |+ a |+ b
            NSub a b  -> play NAdd |+ a |+ b
            NMul a b  -> play NAdd |+ a |+ b
            NDiv a b  -> play NAdd |+ a |+ b
