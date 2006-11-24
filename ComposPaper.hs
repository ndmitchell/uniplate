
module ComposPaper where

import Data.PlayEx
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad.State


-- SECTION 2
data Exp2 = EAbs2 String Exp2
          | EApp2 Exp2 Exp2
          | EVar2 String
          deriving Show

exp2_1 = EApp2 (EVar2 "a") (EAbs2 "b" (EVar2 "b"))



instance Play Exp2 where
    replaceChildren x =
        case x of
            EAbs2 a c -> playOne (EAbs2 a) c
            EApp2 c1 c2 -> playTwo EApp2 c1 c2
            x -> playDefault x



rename2 :: Exp2 -> Exp2
rename2 = mapUnder $ \e -> case e of
    EAbs2 s x -> EAbs2 ("_" ++ s) x
    EVar2 s -> EVar2 ("_" ++ s)
    x -> x


-- SECTION 3
free2 :: Exp2 -> [String]
free2 = fold (nub . concat) $ \e -> case e of
    EAbs2 s x -> delete s
    EVar2 s -> const [s]
    x -> id



-- SECTION 3.1

-- the direct translation
fresh2 :: Exp2 -> Exp2
fresh2 x = evalState (f [] x) names
    where
        names = ["_" ++ show n | n <- [0..]]
        f vs t = case t of
            EAbs2 x b -> do
                y:fs <- get
                put fs
                liftM (EAbs2 y) (f ((x,y):vs) b)
            EVar2 x ->
                return (EVar2 (fromMaybe x (lookup x vs)))
            _ -> composM (f vs) t


-- SECTION 4

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

stm_1 = SBlock [SDecl T_int (V "x")
               ,SAss (V "x") (EInt 12)
               ,SAss (V "x") (EAdd (EVar (V "y")) (EAdd (EAdd (EInt 1) (EInt 2)) (EInt 3)))
               ,SReturn (EVar (V "y"))
               ]


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
    replaceChildrenEx = playSelf


instance PlayEx Stm Exp where
    replaceChildrenEx x =
        case x of
            SAss x y -> playOne (SAss x) y
            SReturn x -> playOne SReturn x
            x -> playExDefault x

instance PlayEx Exp Stm where
    replaceChildrenEx x =
        case x of
            EStm x -> playOne EStm x
            x -> playExDefault x


instance PlayEx Stm Var where
    replaceChildrenEx x =
        case x of
            SDecl typ var -> playOne (SDecl typ) var
            SAss var e -> (var:collect, \(var:xs) -> SAss var (generate xs))
                where (collect,generate) = replaceChildrenEx e
            SReturn e -> playMore SReturn e
            x -> playExDefault x


instance PlayEx Exp Var where
    replaceChildrenEx x =
        case x of
            EStm x -> playMore EStm x
            EVar x -> playOne EVar x
            x -> playExDefault x

            

rename :: PlayEx x Var => x -> x
rename = mapUnderEx $ \(V x) -> V ("_" ++ x)


warnAssign :: PlayEx x Stm => x -> IO ()
warnAssign x = putStr [chr 7 | SAss{} <- allOverEx x]

symbols :: PlayEx x Stm => x -> [(Var,Typ)]
symbols x = [(v,t) | SDecl t v <- allOverEx x]

constFold :: PlayEx x Exp => x -> x
constFold = mapUnderEx $ \e -> case e of
    EAdd (EInt n) (EInt m) -> EInt (n+m)
    x -> x
