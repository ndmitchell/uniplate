{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

module Data.Generics.PlayInternal(unsafeCast, inlinePerformIO) where

#ifdef __GLASGOW_HASKELL__

import GHC.Exts
import Data.ByteString.Base

unsafeCast :: a -> b
unsafeCast = unsafeCoerce#


#else

import Data.Typeable
import Data.Maybe
import Foreign

unsafeCast :: (Typeable a, Typeable b) => a -> b
unsafeCast = fromJust . cast

inlinePerformIO :: IO a -> a
inlinePerformIO = unsafePerformIO

#endif
