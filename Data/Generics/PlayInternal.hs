{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

module Data.Generics.PlayInternal(unsafeCast) where

#ifdef __GLASGOW_HASKELL__

import GHC.Exts(unsafeCoerce#)

unsafeCast :: a -> b
unsafeCast = unsafeCoerce#

#else

import Data.Typeable
import Data.Maybe

unsafeCast :: (Typeable a, Typeable b) => a -> b
unsafeCast = fromJust . cast

#endif
