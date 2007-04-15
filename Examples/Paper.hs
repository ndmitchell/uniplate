
module Examples.Paper where

import Data.Generics.PlayEx
import Control.Monad.State


data Expr  =  Val  Int         -- a literal value
           |  Var  String      -- a variable
           |  Neg  Expr        -- negation
           |  Add  Expr  Expr  -- addition
           |  Sub  Expr  Expr  -- subtraction
           |  Mul  Expr  Expr  -- multiplication
           |  Div  Expr  Expr  -- division
           deriving (Eq, Show)

{-
instance Play Expr where
    replaceChildren x =
        case x of
            Neg  a    -> ([a]    , \[a']     -> Neg  a'     )
            Add  a b  -> ([a,b]  , \[a',b']  -> Add  a' b'  )
            Sub  a b  -> ([a,b]  , \[a',b']  -> Sub  a' b'  )
            Mul  a b  -> ([a,b]  , \[a',b']  -> Mul  a' b'  )
            Div  a b  -> ([a,b]  , \[a',b']  -> Div  a' b'  )
            _         -> ([]     , \[]       -> x           )
-}

instance PlayEx Expr Expr where
    replaceType x = playSelf x

instance Play a => PlayEx Expr a where
    replaceType x =
        case x of
            Val a    -> play Val |- a
            Var a    -> play Var |- a
            Neg a    -> play Neg |+ a
            Add a b  -> play Add |+ a |+ b
            Sub a b  -> play Add |+ a |+ b
            Mul a b  -> play Add |+ a |+ b
            Div a b  -> play Add |+ a |+ b

instance Play Expr where
    replaceChildren x =
        case x of
            Val a    -> play Val |- a
            Var a    -> play Var |- a
            Neg a    -> play Neg |+ a
            Add a b  -> play Add |+ a |+ b
            Sub a b  -> play Add |+ a |+ b
            Mul a b  -> play Add |+ a |+ b
            Div a b  -> play Add |+ a |+ b






variables_bad :: Expr -> [String]
variables_bad (Var  x    ) = [x]
variables_bad (Val  x    ) = []
variables_bad (Neg  x    ) = variables_bad x
variables_bad (Add  x y  ) = variables_bad x ++ variables_bad y
variables_bad (Sub  x y  ) = variables_bad x ++ variables_bad y
variables_bad (Mul  x y  ) = variables_bad x ++ variables_bad y
variables_bad (Div  x y  ) = variables_bad x ++ variables_bad y


variables :: Expr -> [String]
variables x = [y | Var y <- everything x]


countDivZero :: Expr -> Int
countDivZero x = length [() | Div _ (Val 0) <- everything x]


simplify x = traverse f x
    where  f (Sub x y)           = Add x (Neg y)
           f (Add x y) | x == y  = Mul (Val 2) x
           f x                   = x


mutants :: Expr -> [Expr]
mutants x =  [gen (Val j)
             | (Val i, gen) <- everythingContext x
             , j <- [i-1, i+1]]


depth :: Expr -> Int
depth = fold (foldr max 0) $ const (+1)


uniqueVars :: Expr -> Expr
uniqueVars x = evalState (traverseM f x) vars
    where
        vars = ['x':show i | i <- [1..]]

        f (Var i)  = do  y:ys <- get
                         put ys
                         return (Var y)
        f x        = return x


