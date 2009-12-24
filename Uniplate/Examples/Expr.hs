
module Examples.Expr where

import Data.Generics.Play
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
hasDivZero x = not $ null [() | Div _ (Val 0) <- everything x]


depth :: Expr -> Int
depth = fold (foldr max 0) $ const (+1)


optimise :: Expr -> Expr
optimise = traverse $ \x -> case x of
    Neg (Val i) -> Val (negate i)
    Add x y | x == y -> Mul x (Val 2)
    x -> x


noNegate :: Expr -> Expr
noNegate x = case x of
    Neg (Val i) -> Val (negate i)
    Neg (Neg x) -> noNegate x
    Neg (Sub a b) -> Sub (noNegate b) (noNegate a)
    Neg (Add a b) -> Add (noNegate $ Neg a) (noNegate $ Neg b)
    Neg (Div a b) -> Div (noNegate $ Neg a) (noNegate b)
    Neg (Mul a b) -> Mul (noNegate $ Neg a) (noNegate b)
    x -> descend noNegate x


uniqueLits :: Expr -> Expr
uniqueLits x = evalState (traverseM f x) [0..]
    where
        f (Val i) = do
            y:ys <- get
            put ys
            return (Val y)
        f x = return x



reverseExpr :: Expr -> Expr
reverseExpr = traverse f
    where
        f x = generate $ reverse collect
            where (collect,generate) = replaceChildren x


mutate :: Expr -> [Expr]
mutate x = concat [[gen $ Val $ i-1, gen $ Val $ i+1] | (Val i, gen) <- everythingContext x]
