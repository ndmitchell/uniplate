
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

{-
Command line options are:

gen - do random generation
expr|stm|par|all - which section to run
#n - number of tests to run
!n - which test to execute
-}

main = getArgs >>= main2

main2 args = case head norm of
    "gen" -> do generateExpr 100 >>= print
                generateStm  100 >>= print
                generatePar  100 >>= print
    "expr" -> expr
    "stm" -> stm
    "par" -> par
    "all" -> expr >> stm >> par
    where
        norm = filter (isAlpha . head) args
        count = head $ [read n | '#':n <- args] ++ [1]
        pick  = head $ [read n | '!':n <- args] ++ [-1]
    
        expr = exec "expr" count pick testsExpr tasksExpr
        stm  = exec "stm"  count pick testsStm  tasksStm
        par  = exec "par"  count pick testsPar  tasksPar


groupOn f xs = groupBy ((==) `on` f) $ sortBy (compare `on` f) xs

on g f x y = g (f x) (f y)

fst3 (a,b,c) = a

exec :: String -> Int -> Int -> [a] -> [(String,String,a -> String)] -> IO ()
exec name count only tsts ops = do
        putStrLn $ "= " ++ map toUpper name ++ " ="
        mapM_ (uncurry $ execTask count tsts) (map f task2)
        putStrLn ""
    where
        task2 = if only == -1 then tasks else [tasks !! only]
        tasks = groupOn fst3 ops
        f xs = (fst3 (head xs), Map.toList $ Map.fromList [(b,c) | (a,b,c) <- xs])


execTask :: Int -> [a] -> String -> [(String,a -> String)] -> IO ()
execTask count tsts name ops | ans == ans = do
        putStrLn $ "== " ++ name ++ " =="
        res <- mapM f ops
        hPutChar stderr '\n'
        showTable $ sortBy (compare `on` snd) $ zip (map fst ops) res
    where
        showTable xs = mapM_ (putStrLn . g) xs
            where
                g (a,b) = a ++ replicate (20 - length a) ' ' ++ "\t" ++
                          replicate (8 - length (show b)) ' ' ++ show b

        ans = map (snd $ head ops) tests
        tests = concat $ replicate count tsts

        f (name, action) = do
            start <- getCPUTime
            when (ans /= map action tests) $ putStrLn $ "FAILED TO MATCH in " ++ name
            end <- getCPUTime
            hPutChar stderr '.'
            return $ (end - start) `div` 1000000000
