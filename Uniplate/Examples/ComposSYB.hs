{-# OPTIONS_GHC -fglasgow-exts #-}

module Examples.ComposSYB(module Examples.ComposSYB, module Data.Generics.PlaySYB) where

import Data.Generics.PlaySYB
import Data.Generics


-- SECTION 2
data Exp2 = EAbs2 String Exp2
          | EApp2 Exp2 Exp2
          | EVar2 String
          deriving (Data,Typeable,Show)

data Stm = SDecl Typ Var
         | SAss  Var Exp
         | SBlock [Stm]
         | SReturn Exp
         deriving (Data,Typeable,Show)

data Exp = EStm Stm
         | EAdd Exp Exp
         | EVar Var
         | EInt Int
         deriving (Data,Typeable,Show)

data Var = V String deriving (Data,Typeable,Show)

data Typ = T_int | T_float deriving (Data,Typeable,Show)
