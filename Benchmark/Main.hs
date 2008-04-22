
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

confidence = let (*) = (,) in
             ["simplify" * 1000
             ,"variables" * 2000
             ,"zeros" * 5000
             ,"constFold" * 150
             ,"rename" * 150
             ,"symbols" * 800
             ,"bill" * 1800
             ,"increase" * 100
             ,"incrone" * 100
             ]

main = getArgs >>= main2

main2 args = case head norm of
    "gen" -> do generateExpr 100 >>= print
                generateStm  100 >>= print
                generatePar  100 >>= print
    "expr" -> go [expr]
    "stm" -> go [stm]
    "par" -> go [par]
    "all" -> go [expr,stm,par]
    where
        norm = filter (isAlpha . head) args
        count = head $ [read n | '#':n <- args] ++ [1]
        pick  = head $ [read n | '!':n <- args] ++ [-1]
    
        expr = exec "expr" count pick testsExpr tasksExpr
        stm  = exec "stm"  count pick testsStm  tasksStm
        par  = exec "par"  count pick testsPar  tasksPar

        go xs = do
            res <- sequence xs
            putStrLn "== SUMMARY =="
            putStr $ showResult $ sumResults $ concat res


groupOn f xs = groupBy ((==) `on` f) $ sortBy (compare `on` f) xs

on g f x y = g (f x) (f y)

fst3 (a,b,c) = a

exec :: String -> Int -> Int -> [a] -> [(String,String,a -> String)] -> IO [Result]
exec name count only tsts ops = do
        putStrLn $ "= " ++ map toUpper name ++ " ="
        res <- mapM (uncurry $ execTask count tsts) (map f task2)
        putStrLn ""
        return res
    where
        task2 = if only == -1 then tasks else [tasks !! only]
        tasks = groupOn fst3 ops
        f xs = (fst3 (head xs), Map.toList $ Map.fromList [(b,c) | (a,b,c) <- xs])


execTask :: Int -> [a] -> String -> [(String,a -> String)] -> IO Result
execTask count tsts name ops | ans == ans = do
        putStrLn $ "== " ++ name ++ " =="
        res <- mapM f ops
        hPutChar stderr '\n'
        let results = normResult $ zip (map fst ops) res
        putStr $ showResult results
        return results
    where
        ans = map (snd $ head ops) tsts
        count2 = if count /= 0 then count else
                 fromMaybe (error $ "No number specified for: " ++ name) $ lookup name confidence

        f (name, action) = do
            start <- getCPUTime
            replicateM_ count2 $ do
                res <- return $ map action tsts
                when (ans /= res) $ do
                    let (skip,(want,got):_) = span (\(a,b) -> a == b) $ zip ans res
                    putStrLn $ "FAILED TO MATCH in " ++ name ++ "\n" ++
                               "After: " ++ show (length skip) ++ "\n" ++
                               "Wanted: " ++ want ++ "\n" ++
                               "Got:    " ++ got ++ "\n"
            end <- getCPUTime
            hPutChar stderr '.'
            return $ (end - start) `div` 1000000000 -- fake resolution



-- a mapping from variants to times
type Result = [(String,Integer)]

normResult :: Result -> Result
normResult xs = [("raw",raw) | raw < 250] ++ map f (filter ((/=) "raw" . fst) xs)
    where
        raw = max (fromMaybe 1 $ lookup "raw" xs) 1
        f (name,val) = (name, (val * 100) `div` raw)


showResult :: Result -> String
showResult xs = unlines $ (++) raw $ map f $ filter ((/=) "raw" . fst) $
                sortBy (compare `on` snd) xs
        where
            raw = case lookup "raw" xs of
                      Nothing -> []
                      Just x -> ["Low confidence: raw = " ++ show x]

            f (name,val) = name ++ pad 10 name ++ "\t" ++ pad 8 v ++ v
                where v = dp2 val

            pad n x = replicate (n - length x) ' '


dp2 :: Integer -> String
dp2 x | x < 0 = error "dp2 on negative number"
      | x < 100 = "0." ++ tail (show $ 100+x)
      | otherwise = reverse b ++ "." ++ reverse a
            where (a,b) = splitAt 2 $ reverse $ show x


sumResults :: [Result] -> Result
sumResults xs@(x:_) = concatMap (f . fst) x
    where
        n = toInteger $ length xs
        f name = [(name, sum (map fromJust found) `div` n) | all isJust found]
            where found = map (lookup name) xs
