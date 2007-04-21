{-# OPTIONS_GHC -fglasgow-exts #-}

module Data where

import Data.Generics


-- * SECTION 1

data Expr  =  Val  Int         -- a literal value
           |  Var  String      -- a variable
           |  Neg  Expr        -- negation
           |  Add  Expr  Expr  -- addition
           |  Sub  Expr  Expr  -- subtraction
           |  Mul  Expr  Expr  -- multiplication
           |  Div  Expr  Expr  -- division
           deriving (Eq,Show,Read)


-- normal version
data NExpr  =  NVal  Int         -- a literal value
            |  NVar  String      -- a variable
            |  NNeg  NExpr        -- negation
            |  NAdd  NExpr  NExpr  -- addition
            |  NSub  NExpr  NExpr  -- subtraction
            |  NMul  NExpr  NExpr  -- multiplication
            |  NDiv  NExpr  NExpr  -- division
            deriving (Eq,Data,Typeable)


data DExpr

-- Compos version
type CExpr = GExpr DExpr

-- GADT version
data GExpr :: * -> * where
    CVal :: Int           -> GExpr DExpr -- a literal value
    CVar :: String        -> GExpr DExpr -- a variable
    CNeg :: CExpr          -> GExpr DExpr -- negation
    CAdd :: CExpr -> CExpr  -> GExpr DExpr -- addition
    CSub :: CExpr -> CExpr  -> GExpr DExpr -- subtraction
    CMul :: CExpr -> CExpr  -> GExpr DExpr -- multiplication
    CDiv :: CExpr -> CExpr  -> GExpr DExpr -- division


instance Eq (GExpr a) where
    (CVal a1) == (CVal a2) = a1 == a2
    (CVar a1) == (CVar a2) = a1 == a2
    (CNeg a1) == (CNeg a2) = a1 == a2
    (CAdd a1 b1) == (CAdd a2 b2) = a1 == a2 && b1 == b2
    (CSub a1 b1) == (CSub a2 b2) = a1 == a2 && b1 == b2
    (CMul a1 b1) == (CMul a2 b2) = a1 == a2 && b1 == b2
    (CDiv a1 b1) == (CDiv a2 b2) = a1 == a2 && b1 == b2
    _ == _ = False



unwrapN :: Expr -> NExpr
unwrapN x = case x of
    Val x -> NVal x
    Var x -> NVar x
    Neg x -> NNeg (unwrapN x)
    Add x y -> NAdd (unwrapN x) (unwrapN y)
    Sub x y -> NSub (unwrapN x) (unwrapN y)
    Mul x y -> NMul (unwrapN x) (unwrapN y)
    Div x y -> NDiv (unwrapN x) (unwrapN y)

rewrapN :: NExpr -> Expr
rewrapN x = case x of
    NVal x -> Val x
    NVar x -> Var x
    NNeg x -> Neg (rewrapN x)
    NAdd x y -> Add (rewrapN x) (rewrapN y)
    NSub x y -> Sub (rewrapN x) (rewrapN y)
    NMul x y -> Mul (rewrapN x) (rewrapN y)
    NDiv x y -> Div (rewrapN x) (rewrapN y)


unwrapC :: Expr -> CExpr
unwrapC x = case x of
    Val x -> CVal x
    Var x -> CVar x
    Neg x -> CNeg (unwrapC x)
    Add x y -> CAdd (unwrapC x) (unwrapC y)
    Sub x y -> CSub (unwrapC x) (unwrapC y)
    Mul x y -> CMul (unwrapC x) (unwrapC y)
    Div x y -> CDiv (unwrapC x) (unwrapC y)

rewrapC :: CExpr -> Expr
rewrapC x = case x of
    CVal x -> Val x
    CVar x -> Var x
    CNeg x -> Neg (rewrapC x)
    CAdd x y -> Add (rewrapC x) (rewrapC y)
    CSub x y -> Sub (rewrapC x) (rewrapC y)
    CMul x y -> Mul (rewrapC x) (rewrapC y)
    CDiv x y -> Div (rewrapC x) (rewrapC y)
