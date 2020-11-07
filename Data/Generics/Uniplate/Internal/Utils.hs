{-# LANGUAGE Rank2Types, MagicHash, UnboxedTuples, ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-} -- SPEC2

-- | Internal module, do not import or use.
module Data.Generics.Uniplate.Internal.Utils(
    unsafeCoerce, builder, unsafePerformIO, inlinePerformIO, concatCont, SPEC(SPEC)
    ) where

import System.IO.Unsafe(unsafePerformIO)
import Unsafe.Coerce(unsafeCoerce)

import GHC.Exts(build, realWorld#)
import GHC.IO(IO(IO))
import GHC.Exts(SpecConstrAnnotation(..))
{-# ANN type SPEC ForceSpecConstr #-}


{-# INLINE builder #-}
-- | GHCs @foldr@\/@build@ system, but on all platforms
builder :: forall a . (forall b . (a -> b -> b) -> b -> b) -> [a]
builder = build


{-# INLINE inlinePerformIO #-}
-- | 'unsafePerformIO', but suitable for inlining. Copied from "Data.ByteString.Base".
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r


{-# INLINE concatCont #-}
-- | Perform concatentation of continuations
concatCont :: [a -> a] -> a -> a
concatCont xs rest = foldr ($) rest xs


-- | Constructor specialisation on newer GHC
data SPEC = SPEC | SPEC2
