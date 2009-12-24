{-# LANGUAGE DeriveDataTypeable #-}

module Uniplate.Type where

import Data.Data

data Expr = Val Int
          | Var String
          | Neg Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
            deriving (Eq, Show, Data, Typeable)

data Stmt = Assign String Expr
          | Sequence [Stmt]
          | If Expr Stmt Stmt
          | While Expr Stmt
            deriving (Eq, Show, Data, Typeable)

{-!
deriving instance PlateTypeable Expr
deriving instance PlateTypeable Stmt

deriving instance PlateDirect Expr
deriving instance PlateDirect Stmt
!-}
