
module OperationsCommon where

import Data


taskExpr :: String -> [(String,Expr -> Res)] -> [(String,String,Expr -> Res)]
taskExpr x y = [(x,a,b) | (a,b) <- y]


data Res = RStrings [String]
         | RInt Int
         | RExpr Expr
         deriving (Show,Eq)


rawWith  f g = ("raw" , f . g . unwrapN)
playWith f g = ("play", f . g . unwrapN)
sybWith  f g = ("syb" , f . g . unwrapN)
compWith f g = ("comp", f . g . unwrapC)

rawStrings  = rawWith  RStrings
playStrings = playWith RStrings
sybStrings  = sybWith  RStrings
compStrings = compWith RStrings

rawInt  = rawWith  RInt
playInt = playWith RInt
sybInt  = sybWith  RInt
compInt = compWith RInt

rawExpr  = rawWith  (RExpr . rewrapN)
playExpr = playWith (RExpr . rewrapN)
sybExpr  = sybWith  (RExpr . rewrapN)
compExpr = compWith (RExpr . rewrapC)


alt s (a,b) = (a ++ " " ++ s,b)
