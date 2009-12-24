{-# OPTIONS_GHC -fglasgow-exts #-}

module DeriveHand where

import Data
import Data.Generics.Biplate
import Data.Generics.Str


instance Uniplate NExpr where
    uniplate x =
        case x of
            NNeg  a    -> (One a                , \(One a')    -> NNeg  a'     )
            NAdd  a b  -> (Two (One a) (One b)  , \(Two (One a') (One b')) -> NAdd  a' b'  )
            NSub  a b  -> (Two (One a) (One b)  , \(Two (One a') (One b')) -> NSub  a' b'  )
            NMul  a b  -> (Two (One a) (One b)  , \(Two (One a') (One b')) -> NMul  a' b'  )
            NDiv  a b  -> (Two (One a) (One b)  , \(Two (One a') (One b')) -> NDiv  a' b'  )
            _          -> (Zero                 , \_         -> x           )

instance Biplate NExpr NExpr where
    biplate x = (Zero, const x)

instance Biplate NStm NVar where
    biplate x =
        case x of
            NSDecl a b -> (One b, \(One b') -> NSDecl a b')
            NSAss a b -> (Two (One a) get, \(Two (One a') bs) -> NSAss a' (gen bs))
                where (get,gen) = biplate b
            NSBlock xs -> (get, \xs -> NSBlock (gen xs))
                where (get,gen) = biplate xs
            NSReturn x -> (get, \xs -> NSReturn (gen xs))
                where (get,gen) = biplate x

instance Uniplate NVar where
    uniplate x = (Zero, \_ -> x)

instance Biplate NExp NVar where
    biplate x =
        case x of
            NEStm x -> (get, \xs -> NEStm (gen xs))
                where (get,gen) = biplate x

            NEAdd x y -> (Two get1 get2, \(Two a b) -> NEAdd (gen1 a) (gen2 b))
                where
                    (get1,gen1) = biplate x
                    (get2,gen2) = biplate y

            NEVar x -> (One x, \(One x') -> NEVar x')
            
            _ -> (Zero, \_ -> x)

instance (Biplate y x, Uniplate x) => Biplate [y] x where
    biplate [] = (Zero, \_ -> [])
    biplate (x:xs) = (Two get1 get2, \(Two a b) -> gen1 a : gen2 b)
        where
            (get1,gen1) = biplate x
            (get2,gen2) = biplate xs

instance Biplate NStm NStm where
    biplate x = (One x, \(One x') -> x')

instance Uniplate NStm where
    uniplate x =
        case x of
            NSAss a b -> (get,\xs -> NSAss a (gen xs))
                where (get,gen) = biplate b
            NSBlock a -> (get,\xs -> NSBlock (gen xs))
                where (get,gen) = biplate a
            NSReturn a -> (get,\xs -> NSReturn (gen xs))
                where (get,gen) = biplate a
            _ -> (Zero, \_ -> x)

instance Biplate NExp NStm where
    biplate x =
        case x of
            NEStm a -> (One a, \(One a') -> NEStm a')
            NEAdd a b -> (Two get1 get2, \(Two a b) -> NEAdd (gen1 a) (gen2 b))
                where
                    (get1,gen1) = biplate a
                    (get2,gen2) = biplate b
            _ -> (Zero, \_ -> x)

instance Biplate NStm NExp where
    biplate x =
        case x of
            NSAss a b -> (One b, \(One b') -> NSAss a b')
            NSReturn a -> (One a, \(One a') -> NSReturn a')
            NSBlock a -> (get,\xs -> NSBlock (gen xs))
                where (get,gen) = biplate a
            _ -> (Zero, \_ -> x)

instance Uniplate NExp where
    uniplate x =
        case x of
            NEStm a -> (get, \xs -> NEStm (gen xs))
                where (get,gen) = biplate a
            NEAdd a b -> (Two (One a) (One b), \(Two (One a') (One b')) -> NEAdd a' b')
            _ -> (Zero, \_ -> x)


instance Biplate NCompany NSalary where
    biplate (NC xs) = (get, \xs -> NC (gen xs))
        where (get,gen) = biplate xs

instance Biplate NDept NSalary where
    biplate (ND a (NE b c) d) = (Two (One c) get, \(Two (One x) xs) -> ND a (NE b x) (gen xs))
        where (get,gen) = biplate d

instance Biplate NUnt NSalary where
    biplate (NPU (NE a b)) = (One b, \(One x) -> NPU (NE a x))
    biplate (NDU x) = (get, \xs -> NDU (gen xs))
        where (get,gen) = biplate x

instance Uniplate NSalary where
    uniplate x = (Zero, \_ -> x)

instance Biplate NCompany NDept where
    biplate (NC x) = (listStr x, NC . strList)

instance Biplate NUnt NDept where
    biplate (NDU x) = (One x, \(One x') -> NDU x')
    biplate x = (Zero, \_ -> x)

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
