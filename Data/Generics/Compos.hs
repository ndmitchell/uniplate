{-|
    Compos compatibility layer. This module serves as a drop-in
    replacement in some situations for some of the Compos operations.
    Only the single-type traversals are supported, on normal
    algebraic data types. Users should also import either "Data.Generics.Uniplate.Data"
    or "Data.Generics.Uniplate.Direct".

    Compos is described in the paper: \"A Pattern for Almost Compositional Functions\"
    by Bjorn Bringert and Aarne Ranta.

    * <http://doi.acm.org/10.1145/1159803.1159834>

    * <http://www.cs.chalmers.se/~bringert/publ/composOp/composOp.pdf>
-}

module Data.Generics.Compos where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Generics.Uniplate.Operations


-- | If you want to keep an existing type class
class Uniplate a => Compos a where


-- | @composOp == 'descend'@
composOp :: Uniplate a => (a -> a) -> a -> a
composOp = descend


-- | @composOpM == 'descendM'@
composOpM :: (Uniplate a, Applicative m) => (a -> m a) -> a -> m a
composOpM = descendM


-- | @composOpM_ == 'composOpFold' (return ()) (>>)@
composOpM_ :: (Uniplate a, Monad m) => (a -> m ()) -> a -> m ()
composOpM_ = composOpFold (return ()) (>>)


-- | @composOpMonoid = 'composOpFold' mempty mappend@
composOpMonoid :: (Uniplate a, Monoid m) => (a -> m) -> a -> m
composOpMonoid = composOpFold mempty mappend


-- | @composOpMPlus = 'composOpFold' mzero mplus@
composOpMPlus :: (Uniplate a, MonadPlus m) => (a -> m b) -> a -> m b
composOpMPlus = composOpFold mzero mplus


-- | Probably replace with 'universe', perhaps 'para'
composOpFold :: Uniplate a => b -> (b -> b -> b) -> (a -> b) -> a -> b
composOpFold zero combine f = foldr combine zero . map f . children
