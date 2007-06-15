{-# OPTIONS_GHC -fglasgow-exts #-}

-- # Typeable
-- # alpha
-- # beta
-- # everywhere'
-- # gmapQ
-- # gmapM
-- # mkT
-- # gfoldl
-- # UniplateOn
-- # composOp
-- # compos
-- # rep
-- # On
-- # Bi
-- # coreSimplify
-- # Data.Generics.PlateTypeable
-- # Data.Generics.PlateDirect
-- # Data.Generics
-- # PlateData
-- # warnAssign
-- # increase
-- # incrOne
-- # Traversable
-- # syb_incrOne
-- # r
-- instance Eq Expr
-- instance Uniplate Expr
-- instance Eq Tree
-- instance Uniplate Tree
-- instance Typeable Expr
-- instance Typeable Stmt
-- instance PlateAll Expr Expr
-- instance PlateAll Stmt Stmt

import Data.List
import Data.Maybe
import Control.Monad.State
import Data.Typeable

i = undefined
n = undefined
etc = undefined
c = undefined
y = undefined
e = undefined
k = undefined
rule = undefined
x = undefined
operate = undefined
e' = undefined
x' = undefined
x_i = undefined
everywhere' = undefined

elipses = undefined

instance Eq (a -> b) where
    _ == _ = True

(==>) a b = not a || b

build :: ((x -> [x] -> [x]) -> [x] -> [x]) -> [x]
build f = f (:) []

