
module Uniplate.Slowdown(slowdown) where

import Uniplate.Type
import Uniplate.Timer
import Control.Exception
import Data.Generics.Uniplate.Data


slowdown :: IO ()
slowdown = do
    putStrLn "Testing for slowdown caused by bug #27"
    mapM_ testN [1..10]


testN :: Int -> IO ()
testN i = do
    t <- timer $ evaluate $ sum [s | S s <- universeBi $ genCom $ i * 10000]
    putStrLn $ "Running with n=" ++ show i ++ " takes " ++ dp2 t ++ "s, ratio " ++ dp2 (t / fromIntegral i)

    
genCom i = C $ take i $ cycle
         [D "Research" ralf [PU joost, PU marlow],
          D "Strategy" blair   []]

ralf = E (P "Ralf" "Amsterdam") (S 8000)
joost = E (P "Joost" "Amsterdam") (S 1000)
marlow = E (P "Marlow" "Cambridge") (S 2000)
blair = E (P "Blair" "London") (S 100000)
