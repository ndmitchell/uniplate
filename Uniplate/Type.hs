{-# LANGUAGE DeriveDataTypeable #-}

module Uniplate.Type where

import Data.Data

data Expr = Val Int
          | Var String
          | Neg Expr
          | Add Expr Expr
            deriving (Eq, Show, Data, Typeable)

data Stmt = Assign String Expr
          | Sequence [Stmt]
          | While Expr Stmt
            deriving (Eq, Show, Data, Typeable)



{-!
deriving instance PlateTypeable Expr
deriving instance PlateTypeable Stmt

deriving instance PlateDirect Expr
deriving instance PlateDirect Stmt
deriving instance PlateDirect Stmt Expr
deriving instance PlateDirect Stmt [Stmt]
deriving instance PlateDirect Stmt Stmt
deriving instance PlateDirect (Either String Int) Int
deriving instance PlateDirect (Either String Int) Char
deriving instance PlateDirect [Stmt]
deriving instance PlateDirect [([Char], Int)] Int
deriving instance PlateDirect ([Char], Int) Int
!-}


