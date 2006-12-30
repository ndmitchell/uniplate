{-# OPTIONS_GHC -fglasgow-exts #-}

module Data where

import Data.Generics


-- common version
data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mul Expr Expr
          | Neg Expr
          deriving (Show,Eq)


-- normal version
data NExpr = NVal Int
           | NAdd NExpr NExpr
           | NSub NExpr NExpr
           | NDiv NExpr NExpr
           | NMul NExpr NExpr
           | NNeg NExpr
           deriving (Typeable, Data)


data DExpr

-- Compos version
type CExpr = GExpr DExpr

-- GADT version
data GExpr :: * -> * where
    GVal :: Int -> GExpr DExpr
    GAdd :: CExpr -> CExpr -> GExpr DExpr
    GSub :: CExpr -> CExpr -> GExpr DExpr
    GDiv :: CExpr -> CExpr -> GExpr DExpr
    GMul :: CExpr -> CExpr -> GExpr DExpr
    GNeg :: CExpr -> GExpr DExpr



unwrapN :: Expr -> NExpr
unwrapN x = case x of
    Val x -> NVal x
    Add x y -> NAdd (unwrapN x) (unwrapN y)
    Sub x y -> NSub (unwrapN x) (unwrapN y)
    Div x y -> NDiv (unwrapN x) (unwrapN y)
    Mul x y -> NMul (unwrapN x) (unwrapN y)
    Neg x -> NNeg (unwrapN x)

rewrapN :: NExpr -> Expr
rewrapN x = case x of
    NVal x -> Val x
    NAdd x y -> Add (rewrapN x) (rewrapN y)
    NSub x y -> Sub (rewrapN x) (rewrapN y)
    NDiv x y -> Div (rewrapN x) (rewrapN y)
    NMul x y -> Mul (rewrapN x) (rewrapN y)
    NNeg x -> Neg (rewrapN x)


unwrapC :: Expr -> CExpr
unwrapC x = case x of
    Val x -> GVal x
    Add x y -> GAdd (unwrapC x) (unwrapC y)
    Sub x y -> GSub (unwrapC x) (unwrapC y)
    Div x y -> GDiv (unwrapC x) (unwrapC y)
    Mul x y -> GMul (unwrapC x) (unwrapC y)
    Neg x -> GNeg (unwrapC x)


rewrapC :: CExpr -> Expr
rewrapC x = case x of
    GVal x -> Val x
    GAdd x y -> Add (rewrapC x) (rewrapC y)
    GSub x y -> Sub (rewrapC x) (rewrapC y)
    GDiv x y -> Div (rewrapC x) (rewrapC y)
    GMul x y -> Mul (rewrapC x) (rewrapC y)
    GNeg x -> Neg (rewrapC x)
