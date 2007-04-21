
module Main where

import System.Environment
import System.CPUTime
import Control.Monad
import Data.List
import qualified Data.Map as Map

import Testset
import Generate
import Data
import OperationsAll



main = getArgs >>= main2

main2 [x] = case x of
    "gen" -> generate 100 >>= print
    "expr" -> exec testset tasksExpr



groupOn f xs = groupBy ((==) `on` f) $ sortBy (compare `on` f) xs
    where on g f x y = g (f x) (f y)

fst3 (a,b,c) = a

exec :: [a] -> [(String,String,a -> Res)] -> IO ()
exec tsts ops = mapM_ (uncurry $ execTask tsts) (map f tasks)
    where
        tasks = groupOn fst3 ops
        f xs = (fst3 (head xs), [(b,c) | (a,b,c) <- xs])


execTask :: [a] -> String -> [(String,a -> Res)] -> IO ()
execTask tsts name ops | ans == ans = do
        putStrLn $ "== " ++ name ++ " =="
        mapM_ f xs
    where
        ans = map (snd $ head ops) tests
        tests = concat $ replicate 100 tsts
        xs = Map.toList $ Map.fromList ops

        f (name, action) = do
            start <- getCPUTime
            when (ans /= map action tests) $ putStrLn $ "FAILED TO MATCH in " ++ name
            end <- getCPUTime
            putStrLn $ name ++ " took \t" ++ show ((end - start) `div` 1000000)

