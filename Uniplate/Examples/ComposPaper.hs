{-# OPTIONS_GHC -fglasgow-exts -fallow-incoherent-instances #-}

module Examples.ComposPaper where


-- any one of the following will work
import Examples.ComposBasic
--import Examples.ComposOverlap
--import Examples.ComposSYB


import Data.List
import Data.Char
import Data.Maybe
import Control.Monad.State


-- SECTION 2
exp2_1 = EApp2 (EVar2 "a") (EAbs2 "b" (EVar2 "b"))


rename2 :: Exp2 -> Exp2
rename2 = traverse $ \e -> case e of
    EAbs2 s x -> EAbs2 ("_" ++ s) x
    EVar2 s -> EVar2 ("_" ++ s)
    x -> x


-- SECTION 3
free2 :: Exp2 -> [String]
free2 = fold (nub . concat) $ \e y -> case e of
    EAbs2 s x -> delete s y
    EVar2 s -> [s]
    x -> y



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
            _ -> descendM (f vs) t


-- SECTION 4

stm_1 = SBlock [SDecl T_int (V "x")
               ,SAss (V "x") (EInt 12)
               ,SAss (V "x") (EAdd (EVar (V "y")) (EAdd (EAdd (EInt 1) (EInt 2)) (EInt 3)))
               ,SReturn (EVar (V "y"))
               ]


-- MANIPULATIONS

rename :: PlayEx x Var => x -> x
rename = traverseEx $ \(V x) -> V ("_" ++ x)


warnAssign :: PlayEx x Stm => x -> IO ()
warnAssign x = putStr [chr 7 | SAss{} <- everythingEx x]

symbols :: PlayEx x Stm => x -> [(Var,Typ)]
symbols x = [(v,t) | SDecl t v <- everythingEx x]

constFold :: PlayEx x Exp => x -> x
constFold = traverseEx $ \e -> case e of
    EAdd (EInt n) (EInt m) -> EInt (n+m)
    x -> x
