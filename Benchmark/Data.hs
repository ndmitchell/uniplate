{-# OPTIONS_GHC -fglasgow-exts #-}

module Data where

import Data.Generics


-- * SECTION 1

data Exp = EAbs String Exp
         | EApp Exp Exp
         | EVar String


-- normal version
data NExp = NAbs String NExp
          | NApp NExp NExp
          | NVar String
          deriving (Typeable, Data)


data DExp

-- Compos version
type CExp = GExp DExp

-- GADT version
data GExp :: * -> * where
    GAbs :: String -> CExp -> GExp DExp
    GApp :: CExp -> CExp -> GExp DExp
    GVar :: String -> GExp DExp



unwrapN :: Exp -> NExp
unwrapN x = case x of
    EAbs x y -> NAbs x (unwrapN y)
    EApp x y -> NApp (unwrapN x) (unwrapN y)
    EVar x -> NVar x

rewrapN :: NExp -> Exp
rewrapN x = case x of
    NAbs x y -> EAbs x (rewrapN y)
    NApp x y -> EApp (rewrapN x) (rewrapN y)
    NVar x -> EVar x


unwrapC :: Exp -> CExp
unwrapC x = case x of
    EAbs x y -> GAbs x (unwrapC y)
    EApp x y -> GApp (unwrapC x) (unwrapC y)
    EVar x -> GVar x


rewrapC :: CExp -> Exp
rewrapC x = case x of
    GAbs x y -> EAbs x (rewrapC y)
    GApp x y -> EApp (rewrapC x) (rewrapC y)
    GVar x -> EVar x
