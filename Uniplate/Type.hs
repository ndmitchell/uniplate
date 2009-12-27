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
deriving instance PlateTypeable Expr
deriving instance PlateTypeable Stm
deriving instance PlateTypeable Exp
deriving instance PlateTypeable Var
deriving instance PlateTypeable Typ
deriving instance PlateTypeable Company
deriving instance PlateTypeable Dept
deriving instance PlateTypeable Unt
deriving instance PlateTypeable Employee
deriving instance PlateTypeable Person
deriving instance PlateTypeable Salary

deriving instance PlateDirect Expr

deriving instance PlateDirect Exp
deriving instance PlateDirect Stm
deriving instance PlateDirect Stm Exp
deriving instance PlateDirect Exp Stm
deriving instance PlateDirect Exp [Stm]
deriving instance PlateDirect Stm [Stm]
deriving instance PlateDirect Stm Stm
deriving instance PlateDirect [Stm]
deriving instance PlateDirect Stm Var
deriving instance PlateDirect Var
deriving instance PlateDirect Exp Var

deriving instance PlateDirect Company Salary
deriving instance PlateDirect Company Dept
deriving instance PlateDirect Dept Salary
deriving instance PlateDirect Salary
deriving instance PlateDirect Dept
deriving instance PlateDirect Employee Salary
deriving instance PlateDirect Unt Salary
deriving instance PlateDirect Unt Dept

deriving instance PlateDirect (Either String Int) Int
deriving instance PlateDirect (Either String Int) Char
deriving instance PlateDirect [([Char], Int)] Int
deriving instance PlateDirect ([Char], Int) Int
!-}
