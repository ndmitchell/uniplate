
module Main where

import System.Environment
import System.CPUTime
import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
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


execTask :: Int -> [a] -> String -> [(String,a -> String)] -> IO Result
execTask count tsts name ops | ans == ans = do
        putStrLn $ "== " ++ name ++ " =="
        res <- mapM f ops
        hPutChar stderr '\n'
        let results = zip (map fst ops) res
        putStr $ showResults results
        return results
    where
        ans = map (snd $ head ops) tests
        tests = concat $ replicate count tsts

        f (name, action) = do
            start <- getCPUTime
            when (ans /= map action tests) $ putStrLn $ "FAILED TO MATCH in " ++ name
            end <- getCPUTime
            hPutChar stderr '.'
            return $ (end - start) `div` 1000000000 -- fake resolution



-- a mapping from variants to times
type Result = [(String,Integer)]

showResults xs = unlines $ (++) rawS $ map f $ filter ((/=) "raw" . fst) $
                 sortBy (compare `on` snd) xs
        where
            raw = fromMaybe 1 $ lookup "raw" xs
            rawS = ["Low confidence: raw = " ++ show raw | raw < 250]

            f (name,val) = name ++ pad 10 name ++ "\t" ++ pad 8 v ++ v
                where
                    pad n x = replicate (n - length x) ' '
                    v = dp2 $ (val * 100) `div` raw 


dp2 :: Integer -> String
dp2 x | x < 0 = error "dp2 on negative number"
      | x < 100 = "0." ++ tail (show $ 100+x)
      | otherwise = reverse b ++ "." ++ reverse a
            where (a,b) = splitAt 2 $ reverse $ show x


sumResults :: [Result] -> Result
sumResults xs@(x:_) = concatMap (f . fst) x
    where
        f name = [(name, sum $ map fromJust found) | all isJust found]
            where found = map (lookup name) xs
