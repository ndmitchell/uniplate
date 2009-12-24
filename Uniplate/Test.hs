
module Uniplate.Test(test) where

import qualified Uniplate.Direct
import qualified Uniplate.Typeable


test = do
    Uniplate.Direct.test
    Uniplate.Typeable.test
    putStrLn "Test successful"
