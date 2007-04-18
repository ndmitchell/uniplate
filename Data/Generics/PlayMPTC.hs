{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

-- this module should not be imported directly

module Data.Generics.PlayMPTC(module Data.Generics.PlayEx, module Data.Generics.PlayMPTC) where

import Data.Generics.PlayEx


instance Play a => PlayEx a a where
    replaceType = playSelf

instance (Play b, PlayAll a b) => PlayEx a b where
    replaceType = replaceAll


