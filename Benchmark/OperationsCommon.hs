
module OperationsCommon where

import Data


taskExpr :: String -> [(String,Expr -> Res)] -> [(String,String,Expr -> Res)]
taskExpr x y = [(x,a,b) | (a,b) <- y]


data Res = RStrings [String]
         | RExpr Expr


rawWith  f g = ("raw" , f . g . unwrapN)
playWith f g = ("play", f . g . unwrapN)
sybWith  f g = ("syb" , f . g . unwrapN)
compWith f g = ("comp", f . g . unwrapC)

rawStrings  = rawWith  RStrings
playStrings = playWith RStrings
sybStrings  = sybWith  RStrings
compStrings = compWith RStrings

