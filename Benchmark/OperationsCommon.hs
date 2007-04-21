
module OperationsCommon where

import Data


task :: Show a => String -> [(String,q -> a)] -> [(String,String,q -> String)]
task x y = [(x,a,\i -> show (b i)) | (a,b) <- y]



rawExpr  f g = ("raw" , f . g . unwrapN)
playExpr f g = ("play", f . g . unwrapN)
sybExpr  f g = ("syb" , f . g . unwrapN)
compExpr f g = ("comp", f . g . unwrapC)


rawExpr2  = rawExpr  rewrapN
playExpr2 = playExpr rewrapN
sybExpr2  = sybExpr  rewrapN
compExpr2 = compExpr rewrapC


rawStm  f g = ("raw" , f . g . unwrapStmN)
playStm f g = ("play", f . g . unwrapStmN)
sybStm  f g = ("syb" , f . g . unwrapStmN)
compStm f g = ("comp", f . g . unwrapStmC)

rawStm2  = rawStm  rewrapStmN
playStm2 = playStm rewrapStmN
sybStm2  = sybStm  rewrapStmN
compStm2 = compStm rewrapStmC

alt s (a,b) = (a ++ " " ++ s,b)
