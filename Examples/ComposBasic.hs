{-# OPTIONS_GHC -fglasgow-exts #-}

module Examples.ComposBasic(module Examples.ComposBasic, module Data.Generics.PlayMPTC) where

import Data.Generics.PlayMPTC


data Exp2 = EAbs2 String Exp2
          | EApp2 Exp2 Exp2
          | EVar2 String
          deriving Show


instance Play Exp2 where
    replaceChildren x =
        case x of
            EAbs2 a c -> playOne (EAbs2 a) c
            EApp2 c1 c2 -> playTwo EApp2 c1 c2
            x -> playDefault x




data Stm = SDecl Typ Var
         | SAss  Var Exp
         | SBlock [Stm]
         | SReturn Exp
         deriving Show

data Exp = EStm Stm
         | EAdd Exp Exp
         | EVar Var
         | EInt Int
         deriving Show

data Var = V String deriving Show

data Typ = T_int | T_float deriving Show



instance Play Stm where
    replaceChildren x =
        case x of
            SBlock x -> (x, SBlock)
            SAss v x -> playMore (SAss v) x
            SReturn x -> playMore SReturn x
            x -> playDefault x


instance Play Exp where
    replaceChildren x =
        case x of
            EStm s -> playMore EStm s
            EAdd a b -> playTwo EAdd a b
            x -> playDefault x

instance Play Var where
    replaceChildren = playDefault



instance PlayEx Stm Stm where
    replaceType = playSelf


instance PlayEx Stm Exp where
    replaceType x =
        case x of
            SAss x y -> playOne (SAss x) y
            SReturn x -> playOne SReturn x
            x -> playExDefault x

instance PlayEx Exp Stm where
    replaceType x =
        case x of
            EStm x -> playOne EStm x
            x -> playExDefault x


instance PlayEx Stm Var where
    replaceType x =
        case x of
            SDecl typ var -> playOne (SDecl typ) var
            SAss var e -> (var:collect, \(var:xs) -> SAss var (generate xs))
                where (collect,generate) = replaceType e
            SReturn e -> playMore SReturn e
            x -> playExDefault x


instance PlayEx Exp Var where
    replaceType x =
        case x of
            EStm x -> playMore EStm x
            EVar x -> playOne EVar x
            x -> playExDefault x

