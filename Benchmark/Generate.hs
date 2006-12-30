
module Generate(generate) where

import Data
import Control.Monad
import System.Random


generate :: Int -> IO [Expr]
generate n = replicateM n generateOne


generateOne :: IO Expr
generateOne = f 0
    where
        f depth = do
            choice <- getStdRandom (randomR (0,5))
            cutoff <- getStdRandom (randomR (0,20))
            if choice <= 0 || cutoff < depth then do
                i <- getStdRandom (randomR (-999,999))
                return $ Val i
             else do
                a <- f (depth+1)
                b <- f (depth+1)
                return $ case choice of
                    1 -> Neg a
                    2 -> Add a b
                    3 -> Sub a b
                    4 -> Mul a b
                    5 -> Div a b
