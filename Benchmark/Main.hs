
module Main where

import System.Environment
import System.CPUTime
import Control.Monad
import Data.List
import Data.Char
import System.IO
import qualified Data.Map as Map

import Testset
import Generate
import Data
import OperationsAll



main = getArgs >>= main2

main2 [x] = case (dropWhile (== '-') x) of
    "gen" -> do generateExpr 100 >>= print
                generateStm  100 >>= print
                generatePar  100 >>= print
    "expr" -> expr
    "stm" -> stm
    "par" -> par
    "all" -> expr >> stm >> par
    where
        expr = exec "expr" (n*10) testsExpr tasksExpr
        stm = exec "stm" (n*3) testsStm tasksStm
        par = exec "par" (n*1) testsPar tasksPar
    
        n = 10 ^ length (takeWhile (== '-') x)


groupOn f xs = groupBy ((==) `on` f) $ sortBy (compare `on` f) xs

on g f x y = g (f x) (f y)

fst3 (a,b,c) = a

exec :: String -> Int -> [a] -> [(String,String,a -> String)] -> IO ()
exec name count tsts ops = do
        hSetBuffering stdout NoBuffering
        putStrLn $ "= " ++ map toUpper name ++ " ="
        mapM_ (uncurry $ execTask count tsts) (map f tasks)
        putStrLn ""
    where
        tasks = groupOn fst3 ops
        f xs = (fst3 (head xs), Map.toList $ Map.fromList [(b,c) | (a,b,c) <- xs])


execTask :: Int -> [a] -> String -> [(String,a -> String)] -> IO ()
execTask count tsts name ops | ans == ans = do
        putStrLn $ "== " ++ name ++ " =="
        res <- mapM f ops
        putChar '\n'
        showTable $ sortBy (compare `on` snd) $ zip (map fst ops) res
    where
        showTable xs = mapM_ (putStrLn . g) xs
            where
                g x@(a,b) = a ++ replicate (4 + width - length (show x)) ' ' ++ show b
                width = maximum $ map (length . show) xs
    
        ans = map (snd $ head ops) tests
        tests = concat $ replicate count tsts

        f (name, action) = do
            start <- getCPUTime
            when (ans /= map action tests) $ putStrLn $ "FAILED TO MATCH in " ++ name
            end <- getCPUTime
            putChar '.'
            return $ (end - start) `div` 1000000
