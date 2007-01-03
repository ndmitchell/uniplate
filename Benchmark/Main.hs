
module Main where

import System.Environment
import System.CPUTime
import Control.Monad

import Testset
import Generate
import Data

import qualified Compos
import qualified Raw
import qualified PlaySYB
import qualified Play


main = getArgs >>= main2


main2 [x] = case x of
    "gen" -> generate 100 >>= print
    "t1" -> exec1 tasks1
    "t2" -> exec1 tasks2



tasks1 = [("Compos", Compos.task1 . unwrapC)
         ,("Raw", Raw.task1 . unwrapN)
         ,("Play Over", Play.task1_over . unwrapN)
         ,("Play Fold", Play.task1_fold . unwrapN)
         ,("Slay Over", PlaySYB.task1_over . unwrapN)
         ,("Slay Fold", PlaySYB.task1_fold . unwrapN)
         ]

tasks2 = [("Compos", rewrapC . Compos.task2 . unwrapC)
         ,("Raw", rewrapN . Raw.task2 . unwrapN)
         ,("Play Compos", rewrapN . Play.task2_compos . unwrapN)
         ,("Play Under" , rewrapN . Play.task2_under  . unwrapN)
         ,("Play Over"  , rewrapN . Play.task2_over   . unwrapN)
         ,("Slay Compos", rewrapN . PlaySYB.task2_compos . unwrapN)
         ,("Slay Under" , rewrapN . PlaySYB.task2_under  . unwrapN)
         ,("Slay Over"  , rewrapN . PlaySYB.task2_over   . unwrapN)
         ]



exec1 :: Eq a => [(String, Expr -> a)] -> IO ()
exec1 tsks | ans == ans = mapM_ f tsks
    where
        tests = concat $ replicate 100 testset
    
        ans = map (snd $ head tsks) tests
        
        f (name, action) = do
            start <- getCPUTime
            when (ans /= map action tests) $ putStrLn $ "FAILED TO MATCH in " ++ name
            end <- getCPUTime
            putStrLn $ name ++ " took \t" ++ show ((end - start) `div` 1000000)

