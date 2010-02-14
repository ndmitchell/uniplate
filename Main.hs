
module Main where

import System.Environment
import Uniplate.Test
import Uniplate.Generate
import Uniplate.Benchmark
import Uniplate.Slowdown
import Uniplate.HSE
import Uniplate.Fusion

main = do
    xs <- getArgs
    case xs of
        ["generate"] -> generate
        ["test"] -> test
        ["benchmark"] -> benchmark
        ["slowdown"] -> slowdown
        ["hse"] -> hse
        ["fusion"] -> fusion
        _ -> error "Must enter exactly one of: generate, test, benchmark, slowdown"
