
module Uniplate.Benchmark(benchmark) where

import qualified Uniplate.SYB as SYB
import qualified Uniplate.Raw as Raw
import qualified Uniplate.Direct as Direct
import qualified Uniplate.Typeable as Typeable
import qualified Uniplate.Data as Data
import Uniplate.Type
import Uniplate.Testset
import Data.List
import Control.Monad
import Uniplate.Timer


benchmark = do
    columns ["Raw","Direct","Typeable","Data","SYB"]
    let bs = [Raw.benchmark, Direct.benchmark, Typeable.benchmark, Data.benchmark, SYB.benchmark]
    r1 <- run bs testsExpr simplify "simplify" 5000
    r2 <- run bs testsExpr variables "variables" 5000
    r3 <- run bs testsExpr zeros "zeros" 8000
    r4 <- run bs testsStm constFold "constFold" 2000
    r5 <- run bs testsStm rename "rename" 2000
    r6 <- run bs testsStm symbols "symbols" 4000
    r7 <- run bs testsPar bill "bill" 5000
    r8 <- run bs testsPar increase "increase" 800
    r9 <- run bs testsPar incrone "incrone" 1500
    line "Totals" $ map sum $ transpose [r1,r2,r3,r4,r5,r6,r7,r8,r9]


colFirst = 15
colRest = 10
pad n xs = replicate (n - length xs) ' ' ++ xs

columns xs = putStrLn $ pad colFirst "" ++ concatMap (pad colRest) xs

line lbl xs = putStrLn $ pad colFirst lbl ++ concatMap (pad colRest . dp2) (map (/ mn) xs ++ [mn])
    where mn = minimum xs

run :: (Show out, Eq out) => [Benchmark] -> [inp] -> (Benchmark -> inp -> out) -> String -> Int -> IO [Double]
run bs inp sel name n = do
    let out = map (sel $ head bs) inp
    ts <- mapM (runOne n inp out . sel) bs
    line name ts
    return ts

runOne :: (Show out, Eq out) => Int -> [inp] -> [out] -> (inp -> out) -> IO Double
runOne n inp out op = timer $ do
    let x === y = x == y || error ("Mismatch with wanted:\n" ++ show y ++ "\nand got:\n" ++ show x)
    let b = all (=== out) $ map (map op) $ replicate n inp
    unless b $ error "Mismatch on answers"
