{-# OPTIONS_GHC -fglasgow-exts #-}

module Compos(task1, task2) where

import Control.Monad.Identity
import Data.Monoid
import Data


task1 :: GExpr a -> Int
task1 x = case x of
    GVal i | i > 0 -> i
    _ -> composOpFold 0 (+) task1 x


task2 :: GExpr a -> GExpr a
task2 x = case x of
    GMul a b -> GAdd (task2 a) (task2 b)
    GNeg a -> task2 a
    _ -> composOp task2 x


instance Compos GExpr where
    compos return ap f t = case t of
        GVal i -> return (GVal i)
        GAdd a b -> return GAdd `ap` f a `ap` f b
        GSub a b -> return GSub `ap` f a `ap` f b
        GDiv a b -> return GDiv `ap` f a `ap` f b
        GMul a b -> return GMul `ap` f a `ap` f b
        GNeg a -> return GNeg `ap` f a

-- stuff from the Compos module

class Compos t where
    compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
                                   -> (forall a. t a -> m (t a)) -> t c -> m (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . composOpM (Identity . f)

composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composOpM = compos return ap

composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()
composOpM_ = composOpFold (return ()) (>>)

composOpMonoid :: (Compos t, Monoid m) => (forall a. t a -> m) -> t c -> m
composOpMonoid = composOpFold mempty mappend

composOpMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b
composOpMPlus = composOpFold mzero mplus

composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)

newtype C b a = C { unC :: b }
