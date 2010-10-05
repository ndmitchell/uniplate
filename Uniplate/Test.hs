
module Uniplate.Test(test) where

import qualified Uniplate.Direct
import qualified Uniplate.Typeable
import qualified Uniplate.Data
import qualified Uniplate.DataOnly


test = do
    Uniplate.Typeable.test "typeable"
    Uniplate.Direct.test "direct"
    Uniplate.Data.test "data"
    Uniplate.DataOnly.test "dataonly"
    putStrLn ""
    putStrLn "Test successful"
