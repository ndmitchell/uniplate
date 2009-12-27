
module Uniplate.SYB(benchmark) where

import Uniplate.Type
import Data.Generics


benchmark :: Benchmark
benchmark = Benchmark
    variables_ zeros_ simplify_
    rename_ symbols_ constFold_
    (increase_ 100) (incrone_ "" 100) bill_


variables_ = everything (++) ([] `mkQ` f)
    where
        f (Var x) = [x]
        f _ = []


zeros_ = everything (+) (0 `mkQ` f)
    where
        f (Div _ (Val 0)) = 1
        f _ = 0



simplify_ = everywhere (mkT simp)
    where
        simp (Sub x y)           = simp $ Add x (Neg y)
        simp (Add x y) | x == y  = Mul (Val 2) x
        simp x                    = x


rename_ = everywhere (mkT rename_op)
    where rename_op (V x) = V ("_" ++ x)


symbols_ = everything (++) ([] `mkQ` f)
    where
        f (SDecl t v) = [(v,t)]
        f _ = []

constFold_ = everywhere (mkT const_op)

const_op (EAdd (EInt n) (EInt m)) = EInt (n+m)
const_op x = x

increase_ k = increase_int k
increase_int k = everywhere (mkT (increase_op k))

increase_op k (S s) = S (s+k)


incrone_ n k = f n k
    where
        f :: Data a => String -> Integer -> a -> a
        f n k a | isDept n a = increase_int k a
                | otherwise = gmapT (f n k) a

        isDept :: Data a => String -> a -> Bool
        isDept n = False `mkQ` isDeptD n

        isDeptD n (D n' _ _) = n==n'

bill_ = everything (+) (0 `mkQ` billS)
    where billS (S x) = x
