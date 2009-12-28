
import Uniplate.Type
import Data.Char
import Data.Ratio
import qualified Data.Map as Map


benchmark :: Benchmark
benchmark = Benchmark
    variables_ zeros_ simplify_
    rename_ symbols_ constFold_
    (increase_ 100) (incrone_ "" 100) bill_


variables_ x = [y | Var y <- universe x]

zeros_ x = length [() | Div _ (Val 0) <- universe x]

simplify_ = transform simp
    where
        simp (Sub x y)          = simp $ Add x (Neg y)
        simp (Add x y) | x == y = Mul (Val 2) x
        simp x                  = x

rename_ = transformBi rename_op
    where rename_op (V x) = V ("_" ++ x)

symbols_ x = [(v,t) | SDecl t v <- universeBi x]

constFold_ = transformBi const_op
    where
        const_op (EAdd (EInt n) (EInt m)) = EInt (n+m)
        const_op x = x

increase_op k (S s) = S (s+k)
increase_ k = transformBi (increase_op k)

incrone_ name k = descendBi $ f name k
    where
        f name k a@(D n _ _) | name == n = increase_ k a
                              | otherwise = descend (f name k) a
bill_ x = sum [x | S x <- universeBi x]



test :: String -> IO ()
test msg = do
    let a === b | a == b = return ()
                | otherwise = error $ "Did not match in " ++ msg ++ ":\n" ++ show a ++ "\n" ++ show b

    let expr1 = Add (Val 1) (Neg (Val 2))
    universe expr1 === [expr1, Val 1, Neg (Val 2), Val 2]
    children expr1 === [Val 1, Neg (Val 2)]
    transform (\x -> case x of Val n -> Val (n+1) ; _ -> x) expr1 === Add (Val 2) (Neg (Val 3))

    let stmt11 = SAss (V "v") (EInt 1)
        stmt121 = SAss (V "x") (EInt 3)
        stmt12 = SReturn (EAdd (EInt 1) (EStm stmt121))
        stmt1 = SBlock [stmt11,stmt12]
    universe stmt1 === [stmt1,stmt11,stmt12,stmt121]
    children stmt1 === [stmt11,stmt12]
    childrenBi stmt1 === [EInt 1,EAdd (EInt 1) (EStm (SAss (V "x") (EInt 3)))]
    [i | EInt i <- universeBi stmt1] === [1,1,3]
    transformBi (const ([] :: [Stm])) stmt1 === SBlock []
    descend (const stmt121) stmt1 === SBlock [stmt121,stmt121]

    let str1 = "neil"
    universe str1 === ["neil","eil","il","l",""]
    children str1 === ["eil"]
    universeBi str1 === "neil"
    transformBi (reverse :: String -> String) str1 === "elin"
    descendBi toUpper str1 === "NEIL"

    let eith1 = Left str1 :: Either String Int
    universeBi eith1 === ([] :: [Int])
    childrenBi eith1 === str1

    let mp1 = [Map.singleton "neil" (1::Int), Map.fromList [("more",3),("test",4)], Map.empty]
    universeBi mp1 === [1::Int,3,4]
    universeBi (transformBi (+(1::Int)) mp1) === [2::Int,4,5]

    let rat1 = 1 % 2 :: Rational
    universe rat1 === [rat1]
    universeBi rat1 === [1::Integer,2::Integer]

-- TO ADD
-- Map/Set, check we go inside
-- Ratio based test
-- Strict fields
-- Infinite type sizing, data Foo a = Foo Int | Bar (Foo (Foo a))

-- should also rewrite PlateData:
-- Info = IntMap (IntMap IntSet)

-- a, b, then a set of those which are definate miss
-- everything else is assumed to be follow
-- then cache the two lookups inside the biplate instances (works even if biplate is missed)

-- IntMap (IntSet, IntSet)

-- The IntMap is indexed by "to"
-- The first set is those "from" items that have been processed
-- The second set is those which result in miss


-- hitTest :: FromTypeRep -> ToTypeRep -> (FromTypeRep -> Bool)
-- return True if it might be contained within




