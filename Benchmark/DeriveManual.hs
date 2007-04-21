{-# OPTIONS_GHC -fglasgow-exts #-}

module DeriveManual where

import Data
import Data.Generics.PlayEx


instance Play NExpr where
    replaceChildren x =
        case x of
            NNeg  a    -> ([a]    , \(a':_)    -> NNeg  a'     )
            NAdd  a b  -> ([a,b]  , \(a':b':_) -> NAdd  a' b'  )
            NSub  a b  -> ([a,b]  , \(a':b':_) -> NSub  a' b'  )
            NMul  a b  -> ([a,b]  , \(a':b':_) -> NMul  a' b'  )
            NDiv  a b  -> ([a,b]  , \(a':b':_) -> NDiv  a' b'  )
            _         -> ([]     , \_         -> x           )


instance PlayEx NStm NVar where
    replaceType x =
        case x of
            NSDecl a b -> ([b], \(b':_) -> NSDecl a b')
            NSAss a b -> (a:get, \(a':bs) -> NSAss a' (gen bs))
                where (get,gen) = replaceType b
            NSBlock xs -> (get, \xs -> NSBlock (gen xs))
                where (get,gen) = replaceType xs
            NSReturn x -> (get, \xs -> NSReturn (gen xs))
                where (get,gen) = replaceType x

instance Play NVar where
    replaceChildren x = ([], \_ -> x)

instance PlayEx NExp NVar where
    replaceType x =
        case x of
            NEStm x -> (get, \xs -> NEStm (gen xs))
                where (get,gen) = replaceType x

            NEAdd x y -> (get1++get2, \xs -> let (a,b) = splitAt (length get1) xs in NEAdd (gen1 a) (gen2 b))
                where
                    (get1,gen1) = replaceType x
                    (get2,gen2) = replaceType y

            NEVar x -> ([x], \(x':_) -> NEVar x')
            
            _ -> ([], \_ -> x)

instance (PlayEx y x, Play x) => PlayEx [y] x where
    replaceType [] = ([], \_ -> [])
    replaceType (x:xs) = (get1++get2, \ys -> let (a,b) = splitAt (length get1) ys in gen1 a : gen2 b)
        where
            (get1,gen1) = replaceType x
            (get2,gen2) = replaceType xs

instance PlayEx NStm NStm where
    replaceType x = ([x], \(x':_) -> x')

instance Play NStm where
    replaceChildren x =
        case x of
            NSAss a b -> (get,\xs -> NSAss a (gen xs))
                where (get,gen) = replaceType b
            NSBlock a -> (get,\xs -> NSBlock (gen xs))
                where (get,gen) = replaceType a
            NSReturn a -> (get,\xs -> NSReturn (gen xs))
                where (get,gen) = replaceType a
            _ -> ([], \_ -> x)

instance PlayEx NExp NStm where
    replaceType x =
        case x of
            NEStm a -> ([a], \(a':_) -> NEStm a')
            NEAdd a b -> (get1++get2, \ys -> let (a,b) = splitAt (length get1) ys in NEAdd (gen1 a) (gen2 b))
                where
                    (get1,gen1) = replaceType a
                    (get2,gen2) = replaceType b
            _ -> ([], \_ -> x)

instance PlayEx NStm NExp where
    replaceType x =
        case x of
            NSAss a b -> ([b], \(b':_) -> NSAss a b')
            NSReturn a -> ([a], \(a':_) -> NSReturn a')
            NSBlock a -> (get,\xs -> NSBlock (gen xs))
                where (get,gen) = replaceType a
            _ -> ([], \_ -> x)

instance Play NExp where
    replaceChildren x =
        case x of
            NEStm a -> (get, \xs -> NEStm (gen xs))
                where (get,gen) = replaceType a
            NEAdd a b -> ([a,b], \(a':b':_) -> NEAdd a' b')
            _ -> ([], \_ -> x)


instance PlayEx NCompany NSalary where
    replaceType (NC xs) = (get, \xs -> NC (gen xs))
        where (get,gen) = replaceType xs

instance PlayEx NDept NSalary where
    replaceType (ND a (NE b c) d) = (c:get, \(x:xs) -> ND a (NE b x) (gen xs))
        where (get,gen) = replaceType d

instance PlayEx NUnt NSalary where
    replaceType (NPU (NE a b)) = ([b], \(x:_) -> NPU (NE a x))
    replaceType (NDU x) = (get, \xs -> NDU (gen xs))
        where (get,gen) = replaceType x

instance Play NSalary where
    replaceChildren x = ([], \_ -> x)

instance PlayEx NCompany NDept where
    replaceType (NC x) = (x, NC)

instance PlayEx NUnt NDept where
    replaceType (NDU x) = ([x], \(x':_) -> NDU x')
    replaceType x = ([], \_ -> x)

instance Play NDept where
    replaceChildren (ND a b c) = (get, \xs -> ND a b (gen xs))
        where (get,gen) = replaceType c

{-
data NCompany = NC [NDept] deriving Show
data NDept = ND String NEmployee [NUnt] deriving Show
data NUnt = NPU NEmployee | NDU NDept deriving Show
data NEmployee = NE NPerson NSalary deriving Show
data NPerson = NP String String deriving Show
data NSalary = NS Float deriving Show
-}
