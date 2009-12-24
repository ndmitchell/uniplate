
import Uniplate.Type
import Data.Char
import qualified Data.Map as Map


test :: String -> IO ()
test msg = do
    let a === b | a == b = return ()
                | otherwise = error $ "Did not match in " ++ msg ++ ":\n" ++ show a ++ "\n" ++ show b

    let expr1 = Add (Val 1) (Neg (Val 2))
    universe expr1 === [expr1, Val 1, Neg (Val 2), Val 2]
    children expr1 === [Val 1, Neg (Val 2)]
    transform (\x -> case x of Val n -> Val (n+1) ; _ -> x) expr1 === Add (Val 2) (Neg (Val 3))

    let stmt11 = Assign "v" (Val 1)
        stmt121 = Assign "x" (Val 3)
        stmt12 = While (Neg (Val 2)) stmt121
        stmt1 = Sequence [stmt11,stmt12]
    universe stmt1 === [stmt1,stmt11,stmt12,stmt121]
    children stmt1 === [stmt11,stmt12]
    childrenBi stmt1 === [Val 1, Neg (Val 2), Val 3]
    universeBi stmt1 === [Val 1, Neg (Val 2), Val 2, Val 3]
    transformBi (const ([] :: [Stmt])) stmt1 === Sequence []
    descend (const stmt121) stmt1 === Sequence [stmt121,stmt121]

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
