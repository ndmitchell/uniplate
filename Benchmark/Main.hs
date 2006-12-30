
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
    "t1" -> exec1



task1s = [("Compos", Compos.task1 . unwrapC)
         ,("Raw", Raw.task1 . unwrapN)
         ,("Play", Play.task1 . unwrapN)
         ,("PlaySYB", PlaySYB.task1 . unwrapN)
         ]



exec1 :: IO ()
exec1 | ans == ans = mapM_ f task1s
    where
        tests = concat $ replicate 1000 testset
    
        ans = map (snd $ head task1s) tests
        
        f (name, action) = do
            start <- getCPUTime
            when (ans /= map action tests) $ putStrLn $ "FAILED TO MATCH in " ++ name
            end <- getCPUTime
            putStrLn $ name ++ " took " ++ show (end - start)

