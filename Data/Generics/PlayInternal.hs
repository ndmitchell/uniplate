{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE CPP #-}

module Data.Generics.PlayInternal(
    unsafeCast, inlinePerformIO, builder, concatCont
    ) where


---------------------------------------------------------------------
-- GHC
{-
#if 0
{-
#endif
-}

import GHC.Exts(unsafeCoerce#, build, realWorld#)
import GHC.IOBase(IO(IO))

{-# INLINE unsafeCast #-}
unsafeCast :: a -> b
unsafeCast = unsafeCoerce#

{-# INLINE builder #-}
builder :: forall a . (forall b . (a -> b -> b) -> b -> b) -> [a]
builder = build

{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

{-
#if 0
-}
#endif
-}



---------------------------------------------------------------------
-- !GHC
{-
#if 0
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
concatCont :: [a -> a] -> a -> a
concatCont xs rest = foldr ($) rest xs

