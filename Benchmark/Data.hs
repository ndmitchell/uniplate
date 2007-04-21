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
           deriving (Eq,Show)


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


-- * SECTION 2


data Stm = SDecl Typ Var
         | SAss  Var Exp
         | SBlock [Stm]
         | SReturn Exp
         deriving (Eq,Show)

data Exp = EStm Stm
         | EAdd Exp Exp
         | EVar Var
         | EInt Int
         deriving (Eq,Show)

data Var = V String
         deriving (Eq,Show)

data Typ = T_int | T_float
         deriving (Eq,Show)


data NStm = NSDecl NTyp NVar
         | NSAss  NVar NExp
         | NSBlock [NStm]
         | NSReturn NExp

data NExp = NEStm NStm
         | NEAdd NExp NExp
         | NEVar NVar
         | NEInt Int

data NVar = NV String

data NTyp = NT_int | NT_float


data CStm; data CExp; data CVar; data CTyp

data CTree :: * -> * where
    CSDecl :: CTree CTyp -> CTree CVar -> CTree CStm
    CSAss :: CTree CVar -> CTree CExp -> CTree CStm
    CSBlock :: [CTree CStm] -> CTree CStm
    CSReturn :: CTree CExp -> CTree CStm
    CEStm :: CTree CStm -> CTree CExp
    CEAdd :: CTree CExp -> CTree CExp -> CTree CExp
    CEVar :: CTree CVar -> CTree CExp
    CEInt :: Int -> CTree CExp
    CV :: String -> CTree CVar
    CT_int :: CTree CTyp
    CT_float :: CTree CTyp


unwrapStmN x = case x of
    SDecl x y -> NSDecl (unwrapTypN x) (unwrapVarN y)
    SAss x y -> NSAss (unwrapVarN x) (unwrapExpN y)
    SBlock x -> NSBlock (map unwrapStmN x)
    SReturn x -> NSReturn (unwrapExpN x)

unwrapExpN x = case x of
    EStm x -> NEStm (unwrapStmN x)
    EAdd x y -> NEAdd (unwrapExpN x) (unwrapExpN y)
    EVar x -> NEVar (unwrapVarN x)
    EInt x -> NEInt x

unwrapVarN (V x) = (NV x)

unwrapTypN x = case x of
    T_int -> NT_int
    T_float -> NT_float


rewrapStmN x = case x of
    NSDecl x y -> SDecl (rewrapTypN x) (rewrapVarN y)
    NSAss x y -> SAss (rewrapVarN x) (rewrapExpN y)
    NSBlock x -> SBlock (map rewrapStmN x)
    NSReturn x -> SReturn (rewrapExpN x)

rewrapExpN x = case x of
    NEStm x -> EStm (rewrapStmN x)
    NEAdd x y -> EAdd (rewrapExpN x) (rewrapExpN y)
    NEVar x -> EVar (rewrapVarN x)
    NEInt x -> EInt x

rewrapVarN (NV x) = (V x)

rewrapTypN x = case x of
    NT_int -> T_int
    NT_float -> T_float



unwrapStmC x = case x of
    SDecl x y -> CSDecl (unwrapTypC x) (unwrapVarC y)
    SAss x y -> CSAss (unwrapVarC x) (unwrapExpC y)
    SBlock x -> CSBlock (map unwrapStmC x)
    SReturn x -> CSReturn (unwrapExpC x)

unwrapExpC x = case x of
    EStm x -> CEStm (unwrapStmC x)
    EAdd x y -> CEAdd (unwrapExpC x) (unwrapExpC y)
    EVar x -> CEVar (unwrapVarC x)
    EInt x -> CEInt x

unwrapVarC (V x) = (CV x)

unwrapTypC x = case x of
    T_int -> CT_int
    T_float -> CT_float


rewrapStmC x = case x of
    CSDecl x y -> SDecl (rewrapTypC x) (rewrapVarC y)
    CSAss x y -> SAss (rewrapVarC x) (rewrapExpC y)
    CSBlock x -> SBlock (map rewrapStmC x)
    CSReturn x -> SReturn (rewrapExpC x)

rewrapExpC x = case x of
    CEStm x -> EStm (rewrapStmC x)
    CEAdd x y -> EAdd (rewrapExpC x) (rewrapExpC y)
    CEVar x -> EVar (rewrapVarC x)
    CEInt x -> EInt x

rewrapVarC (CV x) = (V x)

rewrapTypC x = case x of
    CT_int -> T_int
    CT_float -> T_float


