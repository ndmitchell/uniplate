
module Main where

import System.Environment
import System.CPUTime
import Control.Monad
import Data.List
import System.IO
import qualified Data.Map as Map

import Testset
import Generate
import Data
import OperationsAll



main = getArgs >>= main2

main2 [x] = case x of
    "gen" -> do generateExpr 100 >>= print
                generateStm  100 >>= print
    "expr" -> exec testsExpr tasksExpr
    "stm" -> exec testsStm tasksStm



groupOn f xs = groupBy ((==) `on` f) $ sortBy (compare `on` f) xs

on g f x y = g (f x) (f y)

fst3 (a,b,c) = a

exec :: [a] -> [(String,String,a -> String)] -> IO ()
exec tsts ops = do
        hSetBuffering stdout NoBuffering
        mapM_ (uncurry $ execTask tsts) (map f tasks)
    where
        tasks = groupOn fst3 ops
        f xs = (fst3 (head xs), Map.toList $ Map.fromList [(b,c) | (a,b,c) <- xs])


execTask :: [a] -> String -> [(String,a -> String)] -> IO ()
execTask tsts name ops | ans == ans = do
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
        tests = concat $ replicate 100 tsts

        f (name, action) = do
            start <- getCPUTime
            when (ans /= map action tests) $ putStrLn $ "FAILED TO MATCH in " ++ name
            end <- getCPUTime
            putChar '.'
            return $ (end - start) `div` 1000000
