{-# LANGUAGE CPP, Rank2Types, MagicHash, UnboxedTuples, ExistentialQuantification #-}

{- |
    Internal module, do not import or use.
-}

module Data.Generics.Uniplate.Internal.Utils(
    unsafeCoerce, builder, unsafePerformIO, inlinePerformIO, concatCont
    ) where

import Foreign(unsafePerformIO)
import Unsafe.Coerce(unsafeCoerce)

#ifdef __GLASGOW_HASKELL__
import GHC.Exts(build, realWorld#)
import GHC.IOBase(IO(IO))
#endif


{-# INLINE builder #-}
-- | GHCs @foldr@\/@build@ system, but on all platforms
#ifdef __GLASGOW_HASKELL__
builder :: forall a . (forall b . (a -> b -> b) -> b -> b) -> [a]
builder = build
#else
builder :: ((x -> [x] -> [x]) -> [x] -> [x]) -> [x]
builder f = f (:) []
#endif


{-# INLINE inlinePerformIO #-}
-- | 'unsafePerformIO', but suitable for inlining. Copied from "Data.ByteString.Base".
inlinePerformIO :: IO a -> a
#ifdef __GLASGOW_HASKELL__
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif


{-# INLINE concatCont #-}
-- | Perform concatentation of continuations
concatCont :: [a -> a] -> a -> a
concatCont xs rest = foldr ($) rest xs
