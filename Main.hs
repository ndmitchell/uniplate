
module Main where

import System.Environment
import Uniplate.Test
import Uniplate.Generate

main = do
    xs <- getArgs
    case xs of
        ["generate"] -> generate
        ["test"] -> test
        _ -> error "Must enter exactly one of: generate, test"
