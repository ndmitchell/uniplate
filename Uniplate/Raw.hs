
module Uniplate.Raw(benchmark) where

import Uniplate.Type


benchmark :: Benchmark
benchmark = Benchmark
    variables_ zeros_ simplify_
    rename_ symbols_ constFold_
    (increase_ 100) (incrone_ "" 100) bill_


variables_ = f
    where
        f (Var  x    ) = [x]
        f (Val  x    ) = []
        f (Neg  x    ) = f x
        f (Add  x y  ) = f x ++ f y
        f (Sub  x y  ) = f x ++ f y
        f (Mul  x y  ) = f x ++ f y
        f (Div  x y  ) = f x ++ f y


zeros_ = f
    where
        f (Div  x (Val 0)) = f x + 1
        f (Var  x    ) = 0
        f (Val  x    ) = 0
        f (Neg  x    ) = f x
        f (Add  x y  ) = f x + f y
        f (Sub  x y  ) = f x + f y
        f (Mul  x y  ) = f x + f y
        f (Div  x y  ) = f x + f y


simplify_ = f
    where
        f (Sub x y) = Add (f x) (Neg (f y))
        f (Add x y) = if x1 == y1 then Mul (Val 2) x1 else Add x1 y1
            where (x1,y1) = (f x,f y)
        f (Mul x y) = Mul (f x) (f y)
        f (Div x y) = Div (f x) (f y)
        f (Neg x) = Neg (f x)
        f x = x


rename_ = f
    where
        f (SDecl a b) = SDecl a (rename_op b)
        f (SAss a b) = SAss (rename_op a) (g b)
        f (SBlock a) = SBlock (map f a)
        f (SReturn a) = SReturn (g a)
        
        g (EStm a) = EStm (f a)
        g (EAdd a b) = EAdd (g a) (g b)
        g (EVar a) = EVar (rename_op a)
        g x = x

rename_op (V x) = V ("_" ++ x)


symbols_ = f
    where
        f (SDecl a b) = [(b,a)]
        f (SAss a b) = g b
        f (SBlock a) = concatMap f a
        f (SReturn a) = g a
        
        g (EStm a) = f a
        g (EAdd a b) = g a ++ g b
        g x = []


constFold_ = f
    where
        f (SAss x y) = SAss x (g y)
        f (SBlock x) = SBlock (map f x)
        f (SReturn x) = SReturn (g x)
        f x = x
        
        g (EStm x) = EStm (f x)
        g (EAdd x y) = case (g x, g y) of
                            (EInt a, EInt b) -> EInt (a+b)
                            (x,y) -> EAdd x y
        g x = x


increase_ k = c
    where
        c (C x) = C $ map d x
        d (D x y z) = D x (e y) (map u z)
        u (PU x) = PU (e x)
        u (DU x) = DU (d x)
        e (E x y) = E x (increase_op k y)


incrone_ n k = c2
    where
        c2 (C x) = C $ map d2 x
        d2 (D x y z) | n == x = D x (e y) (map u z)
        d2 (D x y z) = D x y (map u2 z)
        u2 (PU x) = PU x
        u2 (DU x) = DU (d2 x)

        d (D x y z) = D x (e y) (map u z)
        u (PU x) = PU (e x)
        u (DU x) = DU (d x)
        e (E x y) = E x (increase_op k y)

increase_op k (S s) = S (s+k)


bill_ = c
    where
        c (C x) = sum (map d x)
        d (D x y z) = e y + sum (map u z)
        u (PU x) = e x
        u (DU x) = d x
        e (E x (S y)) = y
