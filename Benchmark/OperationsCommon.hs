
module OperationsCommon where

import Data


task :: Show a => String -> [(String,q -> a)] -> [(String,String,q -> String)]
task x y = [(x,a,\i -> show (b i)) | (a,b) <- y]

alt s (a,b) = (a ++ " " ++ s,b)


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


rawPar  f g = ("raw" , f . g . unwrapCN)
playPar f g = ("play", f . g . unwrapCN)
sybPar  f g = ("syb" , f . g . unwrapCN)
compPar f g = ("comp", f . g . unwrapCC)

rawPar2  = rawPar  rewrapCN
playPar2 = playPar rewrapCN
sybPar2  = sybPar  rewrapCN
compPar2 = compPar rewrapCC

