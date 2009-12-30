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
          deriving (Eq,Show,Data,Typeable)

data Stm = SDecl Typ Var
         | SAss  Var Exp
         | SBlock [Stm]
         | SReturn Exp
         deriving (Eq,Show,Data,Typeable)

data Exp = EStm Stm
         | EAdd Exp Exp
         | EVar Var
         | EInt Int
         deriving (Eq,Show,Data,Typeable)

data Var = V String
         deriving (Eq,Show,Data,Typeable)

data Typ = T_int | T_float
         deriving (Eq,Show,Data,Typeable)

data Company = C [Dept] deriving (Eq,Show,Data,Typeable)
data Dept = D String Employee [Unt] deriving (Eq,Show,Data,Typeable)
data Unt = PU Employee | DU Dept deriving (Eq,Show,Data,Typeable)
data Employee = E Person Salary deriving (Eq,Show,Data,Typeable)
data Person = P String String deriving (Eq,Show,Data,Typeable)
data Salary = S Integer deriving (Eq,Show,Data,Typeable)


data Benchmark = Benchmark
    {variables :: Expr -> [String]
    ,zeros :: Expr -> Int
    ,simplify :: Expr -> Expr
    ,rename :: Stm -> Stm
    ,symbols :: Stm -> [(Var,Typ)]
    ,constFold :: Stm -> Stm
    ,increase :: Company -> Company
    ,incrone :: Company -> Company
    ,bill :: Company -> Integer}


{-!
deriving instance UniplateTypeable Expr
deriving instance UniplateTypeable Stm
deriving instance UniplateTypeable Exp
deriving instance UniplateTypeable Var
deriving instance UniplateTypeable Typ
deriving instance UniplateTypeable Company
deriving instance UniplateTypeable Dept
deriving instance UniplateTypeable Unt
deriving instance UniplateTypeable Employee
deriving instance UniplateTypeable Person
deriving instance UniplateTypeable Salary

deriving instance UniplateDirect Expr

deriving instance UniplateDirect Exp
deriving instance UniplateDirect Stm
deriving instance UniplateDirect Stm Exp
deriving instance UniplateDirect Exp Stm
deriving instance UniplateDirect Exp [Stm]
deriving instance UniplateDirect Stm [Stm]
deriving instance UniplateDirect Stm Stm
deriving instance UniplateDirect [Stm]
deriving instance UniplateDirect Stm Var
deriving instance UniplateDirect Var
deriving instance UniplateDirect Exp Var

deriving instance UniplateDirect Company Salary
deriving instance UniplateDirect Company Dept
deriving instance UniplateDirect Dept Salary
deriving instance UniplateDirect Salary
deriving instance UniplateDirect Dept
deriving instance UniplateDirect Employee Salary
deriving instance UniplateDirect Unt Salary
deriving instance UniplateDirect Unt Dept

deriving instance UniplateDirect (Either String Int) Int
deriving instance UniplateDirect (Either String Int) Char
deriving instance UniplateDirect [([Char], Int)] Int
deriving instance UniplateDirect ([Char], Int) Int
!-}
