{-# OPTIONS_GHC -fglasgow-exts #-}

module DeriveHand where

import Data
import Data.Generics.Biplate


instance Uniplate NExpr where
    uniplate x =
        case x of
            NNeg  a    -> ([a]    , \(a':_)    -> NNeg  a'     )
            NAdd  a b  -> ([a,b]  , \(a':b':_) -> NAdd  a' b'  )
            NSub  a b  -> ([a,b]  , \(a':b':_) -> NSub  a' b'  )
            NMul  a b  -> ([a,b]  , \(a':b':_) -> NMul  a' b'  )
            NDiv  a b  -> ([a,b]  , \(a':b':_) -> NDiv  a' b'  )
            _         -> ([]     , \_         -> x           )


instance Biplate NStm NVar where
    biplate x =
        case x of
            NSDecl a b -> ([b], \(b':_) -> NSDecl a b')
            NSAss a b -> (a:get, \(a':bs) -> NSAss a' (gen bs))
                where (get,gen) = biplate b
            NSBlock xs -> (get, \xs -> NSBlock (gen xs))
                where (get,gen) = biplate xs
            NSReturn x -> (get, \xs -> NSReturn (gen xs))
                where (get,gen) = biplate x

instance Uniplate NVar where
    uniplate x = ([], \_ -> x)

instance Biplate NExp NVar where
    biplate x =
        case x of
            NEStm x -> (get, \xs -> NEStm (gen xs))
                where (get,gen) = biplate x

            NEAdd x y -> (get1++get2, \xs -> let (a,b) = splitAt (length get1) xs in NEAdd (gen1 a) (gen2 b))
                where
                    (get1,gen1) = biplate x
                    (get2,gen2) = biplate y

            NEVar x -> ([x], \(x':_) -> NEVar x')
            
            _ -> ([], \_ -> x)

instance (Biplate y x, Uniplate x) => Biplate [y] x where
    biplate [] = ([], \_ -> [])
    biplate (x:xs) = (get1++get2, \ys -> let (a,b) = splitAt (length get1) ys in gen1 a : gen2 b)
        where
            (get1,gen1) = biplate x
            (get2,gen2) = biplate xs

instance Biplate NStm NStm where
    biplate x = ([x], \(x':_) -> x')

instance Uniplate NStm where
    uniplate x =
        case x of
            NSAss a b -> (get,\xs -> NSAss a (gen xs))
                where (get,gen) = biplate b
            NSBlock a -> (get,\xs -> NSBlock (gen xs))
                where (get,gen) = biplate a
            NSReturn a -> (get,\xs -> NSReturn (gen xs))
                where (get,gen) = biplate a
            _ -> ([], \_ -> x)

instance Biplate NExp NStm where
    biplate x =
        case x of
            NEStm a -> ([a], \(a':_) -> NEStm a')
            NEAdd a b -> (get1++get2, \ys -> let (a,b) = splitAt (length get1) ys in NEAdd (gen1 a) (gen2 b))
                where
                    (get1,gen1) = biplate a
                    (get2,gen2) = biplate b
            _ -> ([], \_ -> x)

instance Biplate NStm NExp where
    biplate x =
        case x of
            NSAss a b -> ([b], \(b':_) -> NSAss a b')
            NSReturn a -> ([a], \(a':_) -> NSReturn a')
            NSBlock a -> (get,\xs -> NSBlock (gen xs))
                where (get,gen) = biplate a
            _ -> ([], \_ -> x)

instance Uniplate NExp where
    uniplate x =
        case x of
            NEStm a -> (get, \xs -> NEStm (gen xs))
                where (get,gen) = biplate a
            NEAdd a b -> ([a,b], \(a':b':_) -> NEAdd a' b')
            _ -> ([], \_ -> x)


instance Biplate NCompany NSalary where
    biplate (NC xs) = (get, \xs -> NC (gen xs))
        where (get,gen) = biplate xs

instance Biplate NDept NSalary where
    biplate (ND a (NE b c) d) = (c:get, \(x:xs) -> ND a (NE b x) (gen xs))
        where (get,gen) = biplate d

instance Biplate NUnt NSalary where
    biplate (NPU (NE a b)) = ([b], \(x:_) -> NPU (NE a x))
    biplate (NDU x) = (get, \xs -> NDU (gen xs))
        where (get,gen) = biplate x

instance Uniplate NSalary where
    uniplate x = ([], \_ -> x)

instance Biplate NCompany NDept where
    biplate (NC x) = (x, NC)

instance Biplate NUnt NDept where
    biplate (NDU x) = ([x], \(x':_) -> NDU x')
    biplate x = ([], \_ -> x)

instance Uniplate NDept where
    uniplate (ND a b c) = (get, \xs -> ND a b (gen xs))
        where (get,gen) = biplate c

{-
data NCompany = NC [NDept] deriving Show
data NDept = ND String NEmployee [NUnt] deriving Show
data NUnt = NPU NEmployee | NDU NDept deriving Show
data NEmployee = NE NPerson NSalary deriving Show
data NPerson = NP String String deriving Show
data NSalary = NS Float deriving Show
-}
