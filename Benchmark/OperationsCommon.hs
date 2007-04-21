
module OperationsCommon where

import Data


task :: String -> [(String,q -> Res)] -> [(String,String,q -> Res)]
task x y = [(x,a,b) | (a,b) <- y]


data Res = RStrings [String]
         | RInt Int
         | RExpr Expr
         | RStm Stm
         deriving (Show,Eq)


rawExpr  f g = ("raw" , f . g . unwrapN)
playExpr f g = ("play", f . g . unwrapN)
sybExpr  f g = ("syb" , f . g . unwrapN)
compExpr f g = ("comp", f . g . unwrapC)


rawExpr2  = rawExpr  (RExpr . rewrapN)
playExpr2 = playExpr (RExpr . rewrapN)
sybExpr2  = sybExpr  (RExpr . rewrapN)
compExpr2 = compExpr (RExpr . rewrapC)


rawStm  f g = ("raw" , f . g . unwrapStmN)
playStm f g = ("play", f . g . unwrapStmN)
sybStm  f g = ("syb" , f . g . unwrapStmN)
compStm f g = ("comp", f . g . unwrapStmC)

rawStm2  = rawStm  (RStm . rewrapStmN)
playStm2 = playStm (RStm . rewrapStmN)
sybStm2  = sybStm  (RStm . rewrapStmN)
compStm2 = compStm (RStm . rewrapStmC)

alt s (a,b) = (a ++ " " ++ s,b)
