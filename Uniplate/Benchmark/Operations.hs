-- This module requires tricky CPP'ing
-- so that you can use 3 different Play instances

module Operations(tasksExpr,tasksStm,tasksPar) where

import Data
import DeriveCompos
import OperationsCommon
import DeriveHand

import Data.Generics.Biplate as Play
import Data.Generics as SYB


tasksExpr = variables ++ zeros ++ simplify
tasksStm = rename ++ symbols ++ constFold
tasksPar = increase ++ incrone ++ bill


-- * SECTION 1


variables = task "variables" [variables_raw, variables_play, {- variables_play2, -} variables_syb, variables_comp]

variables_raw = rawExpr id f
    where
        f (NVar  x    ) = [x]
        f (NVal  x    ) = []
        f (NNeg  x    ) = f x
        f (NAdd  x y  ) = f x ++ f y
        f (NSub  x y  ) = f x ++ f y
        f (NMul  x y  ) = f x ++ f y
        f (NDiv  x y  ) = f x ++ f y


variables_play = playExpr id $ \x -> [y | NVar y <- Play.universe x]

{-
variables_play2 = alt "fold" $ playExpr id $ fold concat f
    where
        f (NVar x) c = x : c
        f _ c = c
-}

variables_syb = sybExpr id $ SYB.everything (++) ([] `mkQ` f)
    where
        f (NVar x) = [x]
        f _ = []

variables_comp = compExpr id f
    where
        f :: GExpr a -> [String]
        f (CVar x) = [x]
        f x = composOpFold [] (++) f x



zeros = task "zeros" [zeros_raw, zeros_play, {- zeros_play2, -} zeros_syb, zeros_comp]

zeros_raw = rawExpr id f
    where
        f (NDiv  x (NVal 0)) = f x + 1
        f (NVar  x    ) = 0
        f (NVal  x    ) = 0
        f (NNeg  x    ) = f x
        f (NAdd  x y  ) = f x + f y
        f (NSub  x y  ) = f x + f y
        f (NMul  x y  ) = f x + f y
        f (NDiv  x y  ) = f x + f y

zeros_play = playExpr id $ \x -> length [() | NDiv _ (NVal 0) <- Play.universe x]

{-
zeros_play2 = alt "fold" $ playExpr id $ fold sum f
    where
        f (NDiv _ (NVal 0)) c = 1 + c
        f _ c = c
-}

zeros_syb = sybExpr id $ SYB.everything (+) (0 `mkQ` f)
    where
        f (NDiv _ (NVal 0)) = 1
        f _ = 0

zeros_comp = compExpr id f
    where
        f :: GExpr a -> Int
        f (CDiv x (CVal 0)) = 1 + f x 
        f x = composOpFold 0 (+) f x



simplify = task "simplify" [simplify_raw,simplify_play, {- simplify_play2, -} simplify_syb,simplify_compos]

simplify_raw = rawExpr2 f
    where
        f (NSub x y) = NAdd (f x) (NNeg (f y))
        f (NAdd x y) = if x1 == y1 then NMul (NVal 2) x1 else NAdd x1 y1
            where (x1,y1) = (f x,f y)
        f (NMul x y) = NMul (f x) (f y)
        f (NDiv x y) = NDiv (f x) (f y)
        f (NNeg x) = NNeg (f x)
        f x = x

simp (NSub x y)           = simp $ NAdd x (NNeg y)
simp (NAdd x y) | x == y  = NMul (NVal 2) x
simp x                    = x

simplify_play = playExpr2 $ transform simp

simplify_play2 = alt "rewrite" $ playExpr2 $ rewrite f
    where
        f (NSub x y)           = Just $ NAdd x (NNeg y)
        f (NAdd x y) | x == y  = Just $ NMul (NVal 2) x
        f x                    = Nothing

simplify_syb = sybExpr2 $ everywhere (mkT simp)

simplify_compos = compExpr2 f
    where
        f :: GExpr a -> GExpr a
        f (CSub x y) = f $ CAdd (f x) (CNeg (f y))
        f (CAdd x y) = if x1 == y1 then CMul (CVal 2) x1 else CAdd x1 y1
            where (x1,y1) = (f x,f y)
        f x = composOp f x



rename = task "rename" [rename_compos, rename_play, rename_syb, rename_raw]

rename_compos = compStm2 f
    where
        f :: CTree c -> CTree c
        f t = case t of
            CV x -> CV ("_" ++ x)
            _ -> composOp f t
            
rename_op (NV x) = NV ("_" ++ x)

rename_play = playStm2 $ transformBi rename_op

rename_syb = sybStm2 $ everywhere (mkT rename_op)

rename_raw = rawStm2 f
    where
        f (NSDecl a b) = NSDecl a (rename_op b)
        f (NSAss a b) = NSAss (rename_op a) (g b)
        f (NSBlock a) = NSBlock (map f a)
        f (NSReturn a) = NSReturn (g a)
        
        g (NEStm a) = NEStm (f a)
        g (NEAdd a b) = NEAdd (g a) (g b)
        g (NEVar a) = NEVar (rename_op a)
        g x = x



symbols = task "symbols" [symbols_compos,symbols_play,symbols_syb,symbols_raw]

rewrapPairC xs = [(rewrapVarC a, rewrapTypC b) | (a,b) <- xs]
rewrapPairN xs = [(rewrapVarN a, rewrapTypN b) | (a,b) <- xs]

symbols_compos = compStm rewrapPairC f
    where
        f :: CTree c -> [(CTree CVar, CTree CTyp)]
        f t = case t of
            CSDecl typ var -> [(var,typ)]
            _ -> composOpMonoid f t

symbols_play = playStm rewrapPairN $ \x -> [(v,t) | NSDecl t v <- universeBi x]

symbols_syb = sybStm rewrapPairN $ SYB.everything (++) ([] `mkQ` f)
    where
        f (NSDecl t v) = [(v,t)]
        f _ = []

symbols_raw = rawStm rewrapPairN f
    where
        f (NSDecl a b) = [(b,a)]
        f (NSAss a b) = g b
        f (NSBlock a) = concatMap f a
        f (NSReturn a) = g a
        
        g (NEStm a) = f a
        g (NEAdd a b) = g a ++ g b
        g x = []



constFold = task "constFold" [constFold_raw, constFold_compos,constFold_play,constFold_syb]

constFold_compos = compStm2 f
    where
        f :: CTree c -> CTree c
        f e = case e of
            CEAdd x y -> case (f x, f y) of
                            (CEInt n, CEInt m) -> CEInt (n+m)
                            (x',y') -> CEAdd x' y'
            _ -> composOp f e

const_op (NEAdd (NEInt n) (NEInt m)) = NEInt (n+m)
const_op x = x

constFold_play = playStm2 $ transformBi const_op

constFold_syb = sybStm2 $ everywhere (mkT const_op)

constFold_raw = rawStm2 f
    where
        f (NSAss x y) = NSAss x (g y)
        f (NSBlock x) = NSBlock (map f x)
        f (NSReturn x) = NSReturn (g x)
        f x = x
        
        g (NEStm x) = NEStm (f x)
        g (NEAdd x y) = case (g x, g y) of
                            (NEInt a, NEInt b) -> NEInt (a+b)
                            (x,y) -> NEAdd x y
        g x = x



increase = task "increase" [increase_play v, increase_syb v, increase_comp v, increase_raw v]
    where v = 100

increase_op k (NS s) = NS (s+k)

increase_play k = playPar2 (increase_play_int k)
increase_play_int k = transformBi (increase_op k)

increase_comp k = compPar2 $ increase_comp_int k
increase_comp_int k = f k
    where
        f :: Integer -> Paradise c -> Paradise c
        f k c = case c of
            CS s -> CS (s+k)
            _ -> composOp (f k) c

increase_syb k = sybPar2 $ increase_syb_int k
increase_syb_int k = everywhere (mkT (increase_op k))

increase_raw k = rawPar2 c
    where
        c (NC x) = NC $ map d x
        d (ND x y z) = ND x (e y) (map u z)
        u (NPU x) = NPU (e x)
        u (NDU x) = NDU (d x)
        e (NE x y) = NE x (increase_op k y)



incrone = task "incrone" [incrone_play n v, incrone_syb n v, incrone_comp n v, incrone_raw n v]
    where v = 100; n = "" -- most common department name

incrone_play name k = playPar2 $ descendBi $ f name k
    where
        f name k a@(ND n _ _) | name == n = increase_play_int k a
                              | otherwise = descend (f name k) a

incrone_syb n k = sybPar2 $ f n k
    where
        f :: Data a => String -> Integer -> a -> a
        f n k a | isDept n a = increase_syb_int k a
                | otherwise = gmapT (f n k) a

        isDept :: Data a => String -> a -> Bool
        isDept n = False `mkQ` isDeptD n

        isDeptD n (ND n' _ _) = n==n'

incrone_comp n k = compPar2 $ f n k
    where
        f :: String -> Integer -> Paradise c -> Paradise c
        f d k c = case c of
            CD n _ _ | n == d -> increase_comp_int k c
            _ -> composOp (f d k) c

incrone_raw n k = rawPar2 c2
    where
        c2 (NC x) = NC $ map d2 x
        d2 (ND x y z) | n == x = ND x (e y) (map u z)
        d2 (ND x y z) = ND x y (map u2 z)
        u2 (NPU x) = NPU x
        u2 (NDU x) = NDU (d2 x)

        d (ND x y z) = ND x (e y) (map u z)
        u (NPU x) = NPU (e x)
        u (NDU x) = NDU (d x)
        e (NE x y) = NE x (increase_op k y)



bill = task "bill" [bill_play,bill_syb,bill_raw,bill_comp]

bill_comp = compPar id f
    where
        f :: Paradise c -> Integer
        f c = case c of
            CS s -> s
            _ -> composOpFold 0 (+) f c

bill_syb = sybPar id $ SYB.everything (+) (0 `mkQ` billS)
    where billS (NS x) = x

bill_play = playPar id $ \x -> sum [x | NS x <- universeBi x]

bill_raw = rawPar id c
    where
        c (NC x) = sum (map d x)
        d (ND x y z) = e y + sum (map u z)
        u (NPU x) = e x
        u (NDU x) = d x
        e (NE x (NS y)) = y
