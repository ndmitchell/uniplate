
module Main where

import System.Environment
import Uniplate.Test
import Uniplate.Generate
import Uniplate.Benchmark
import Uniplate.Slowdown

main = do
    xs <- getArgs
    case xs of
        ["generate"] -> generate
        ["test"] -> test
        ["benchmark"] -> benchmark
        ["slowdown"] -> slowdown
        _ -> error "Must enter exactly one of: generate, test, benchmark, slowdown"
