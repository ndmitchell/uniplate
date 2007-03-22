{-# OPTIONS_GHC -fglasgow-exts #-}

module Examples.SYB where

import Data.Generics
import Data.Generics.PlaySYB

data Company = C [Dept]
    deriving (Typeable,Data)
data Dept = D Name Manager [Unt]
    deriving (Typeable,Data)
data Unt = PU Employee | DU Dept
    deriving (Typeable,Data)
data Employee = E Person Salary
    deriving (Typeable,Data)
data Person = P Name Address
    deriving (Typeable,Data)
data Salary = S Float
    deriving (Typeable,Data)
type Manager = Employee
type Name = String
type Address = String



increase :: PlayEx x Salary => Float -> x -> x
increase k = traverseEx (\(S s) -> S (s * (1+k)))

incrOne :: PlayEx x Dept => String -> Float -> x -> x
incrOne name k = traverseEx (\d@(D n _ _) -> if name == n then increase k d else d)

salaryBill :: PlayEx x Salary => x -> Float
salaryBill x = sum [x | S x <- everythingEx x]

salaryBill2 :: PlayEx x Salary => x -> Float
salaryBill2 = sum . map billS . everythingEx


billS :: Salary -> Float
billS (S f) = f
