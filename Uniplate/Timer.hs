
module Uniplate.Timer(getTime, timer, dp2) where

import Data.Time.Clock.POSIX(getPOSIXTime)
import Numeric

getTime :: IO Double
getTime = (fromRational . toRational) `fmap` getPOSIXTime


timer :: IO a -> IO Double
timer x = do
    start <- getTime
    x
    stop <- getTime
    return $ stop - start

dp2 x = showFFloat (Just 2) x ""
