
module ExprExamples where

import Data.Play


data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mul Expr Expr
          | Neg Expr
          deriving (Show,Eq)


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
depth = fold (foldr max 0) $ \_ d -> d+1


optimise :: Expr -> Expr
optimise = mapUnder $ \x -> case x of
    Neg (Val i) -> Val (negate i)
    Add x y | x == y -> Mul x (Val 2)
    x -> x


reverseExpr :: Expr -> Expr
reverseExpr = mapUnder f
    where
        f x = generate $ reverse collect
            where (collect,generate) = replaceChildren x
