{-# OPTIONS_GHC -fglasgow-exts -fallow-incoherent-instances #-}

module Examples.ComposOverlap(module Examples.ComposOverlap, module Data.Generics.PlayMPTC) where

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


-- COMBINATOR BASED PLAY INSTANCES
instance PlayEx Stm Stm where; replaceType = playSelf
instance PlayEx Exp Exp where; replaceType = playSelf
instance PlayEx Var Var where; replaceType = playSelf

instance Play a => PlayEx Exp a where
    replaceType x =
        case x of
            EStm a -> play EStm /\ a
            EAdd a b -> play EAdd /\ a /\ b
            EVar a -> play EVar /\ a
            EInt a -> play EInt /\! a


instance Play a => PlayEx Stm a where
    replaceType x =
        case x of
            SDecl a b -> play SDecl /\! a /\ b
            SAss a b -> play SAss /\ a /\ b
            SBlock a -> play SBlock /\ a
            SReturn x -> play SReturn /\ x

instance Play a => PlayEx Var a where
    replaceType = playDefault

