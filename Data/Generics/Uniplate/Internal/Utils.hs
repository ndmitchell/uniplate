{-# LANGUAGE CPP, Rank2Types, MagicHash, UnboxedTuples, ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-} -- SPEC2

-- | Internal module, do not import or use.
module Data.Generics.Uniplate.Internal.Utils(
    unsafeCoerce, builder, unsafePerformIO, inlinePerformIO, concatCont, SPEC(SPEC)
    ) where

#if __GLASGOW_HASKELL__ >= 702
import System.IO.Unsafe(unsafePerformIO)
#else
import Foreign(unsafePerformIO)
#endif
import Unsafe.Coerce(unsafeCoerce)

#ifdef __GLASGOW_HASKELL__
import GHC.Exts(build, realWorld#)
#if __GLASGOW_HASKELL__ < 612
import GHC.IOBase(IO(IO))
#else
import GHC.IO(IO(IO))
#endif
#endif

#if __GLASGOW_HASKELL__ >= 701
import GHC.Exts(SpecConstrAnnotation(..))
{-# ANN type SPEC ForceSpecConstr #-}
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


-- | Constructor specialisation on newer GHC
data SPEC = SPEC | SPEC2
