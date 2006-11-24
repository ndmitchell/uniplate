
module ExprExamples where

import Data.Play
import Control.Monad.State


data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mul Expr Expr
          | Neg Expr
          deriving (Show,Eq)


expr1 = Add (Val 1) (Add (Val 2) (Val 3))


instance Play Expr where
    replaceChildren x =
        case x of
            Add x y -> playTwo Add x y
            Sub x y -> playTwo Sub x y
            Div x y -> playTwo Div x y
            Mul x y -> playTwo Mul x y
            Neg x   -> playOne Neg x
            x -> playDefault x


eval :: Expr -> Int
eval = fold id $ \op args ->
    case (op,args) of
        (Val n, []) -> n
        (Neg x, [i]) -> negate i
        (_, [a,b]) ->
            (case op of
                Add{} -> (+)
                Sub{} -> (-)
                Div{} -> div
                Mul{} -> (*)
            ) a b


hasDivZero :: Expr -> Bool
hasDivZero x = not $ null [() | Div _ (Val 0) <- allOver x]


depth :: Expr -> Int
depth = fold (foldr max 0) $ const (+1)


optimise :: Expr -> Expr
optimise = mapUnder $ \x -> case x of
    Neg (Val i) -> Val (negate i)
    Add x y | x == y -> Mul x (Val 2)
    x -> x


noNegate :: Expr -> Expr
noNegate = mapOver $ \x -> case x of
    Neg (Val i) -> Val (negate i)
    Neg (Neg x) -> x
    Neg (Sub a b) -> Sub b a
    Neg (Add a b) -> Add (Neg a) (Neg b)
    Neg (Div a b) -> Div (Neg a) b
    Neg (Mul a b) -> Mul (Neg a) b
    x -> x


uniqueLits :: Expr -> Expr
uniqueLits x = evalState (mapUnderM f x) [0..]
    where
        f (Val i) = do
            y:ys <- get
            put ys
            return (Val y)
        f x = return x



reverseExpr :: Expr -> Expr
reverseExpr = mapUnder f
    where
        f x = generate $ reverse collect
            where (collect,generate) = replaceChildren x
