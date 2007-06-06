{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE CPP, Rank2Types #-}

{- |
    Internal module, do not import or use.
-}

module Data.Generics.PlateInternal(
    unsafeCast, inlinePerformIO, builder, concatCont
    ) where


---------------------------------------------------------------------
-- GHC
{-
#if !__GLASGOW_HASKELL__
{-
#endif
-}

import GHC.Exts(unsafeCoerce#, build, realWorld#)
import GHC.IOBase(IO(IO))

{-# INLINE unsafeCast #-}
-- | @unsafeCoerce@, but for all compilers. In future this can be obtained from
-- @Unsafe.Coerce.unsafeCoerce@, but thats too recent a change.
unsafeCast :: a -> b
unsafeCast = unsafeCoerce#

{-# INLINE builder #-}
-- | GHCs @foldr@\/@build@ system, but on all platforms
builder :: forall a . (forall b . (a -> b -> b) -> b -> b) -> [a]
builder = build

{-# INLINE inlinePerformIO #-}
-- | 'unsafePerformIO', but suitable for inlining. Copied from "Data.ByteString.Base".
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

{-
#if !__GLASGOW_HASKELL__
-}
#endif
-}



---------------------------------------------------------------------
-- !GHC
{-
#if !__GLASGOW_HASKELL__
-}

import Data.Typeable
import Data.Maybe
import Foreign

unsafeCast :: (Typeable a, Typeable b) => a -> b
unsafeCast = fromJust . cast

inlinePerformIO :: IO a -> a
inlinePerformIO = unsafePerformIO

builder :: ((x -> [x] -> [x]) -> [x] -> [x]) -> [x]
builder f = f (:) []

{-
#endif
-}




{-# INLINE concatCont #-}
-- | Perform concatentation of continuations
concatCont :: [a -> a] -> a -> a
concatCont xs rest = foldr ($) rest xs

