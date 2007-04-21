{-# OPTIONS_GHC -fglasgow-exts #-}

module DeriveCompos where

import Data
import Data.Monoid
import Control.Monad.Identity


instance Compos GExpr where
    compos return ap f t = case t of
        CNeg x -> return CNeg `ap` f x
        CAdd x y -> return CAdd `ap` f x `ap` f y
        CSub x y -> return CSub `ap` f x `ap` f y
        CMul x y -> return CMul `ap` f x `ap` f y
        CDiv x y -> return CDiv `ap` f x `ap` f y
        x -> return x


instance Compos CTree where
    compos return ap f t = case t of
            CSDecl x y -> return CSDecl `ap` f x `ap` f y
            CSAss x y -> return CSAss `ap` f x `ap` f y
            CSBlock xs -> return CSBlock `ap` mapM f xs
            CSReturn x -> return CSReturn `ap` f x
            CEAdd x y -> return CEAdd `ap` f x `ap` f y
            CEStm x -> return CEStm `ap` f x
            CEVar x -> return CEVar `ap` f x
            _ -> return t
        where
            mapM g = foldr (ap . ap (return (:)) . g) (return [])

instance Compos Paradise where
    compos return ap f t = case t of
            CC xs -> return CC `ap` mapM f xs
            CD x y z -> return CD `ap` return x `ap` f y `ap` mapM f z
            CPU x -> return CPU `ap` f x
            CDU x -> return CDU `ap` f x
            CE x y -> return CE `ap` f x `ap` f y
            _ -> return t
        where
            mapM g = foldr (ap . ap (return (:)) . g) (return [])


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
composOpFold z c f = unC . compos (\_ -> Comp z) (\(Comp x) (Comp y) -> Comp (c x y)) (Comp . f)

newtype Comp b a = Comp { unC :: b }
