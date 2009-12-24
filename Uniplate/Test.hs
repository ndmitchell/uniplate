
module Uniplate.Test(test) where

import qualified Uniplate.Direct
import qualified Uniplate.Typeable
import qualified Uniplate.Data
import qualified Uniplate.DataOnly


test = do
    Uniplate.Direct.test
    Uniplate.Typeable.test
    Uniplate.Data.test
    Uniplate.DataOnly.test
    putStrLn "Test successful"
