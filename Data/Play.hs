
module Data.Play where

import Control.Monad


-- THE CLASS
class Play on where
    replaceChildren :: on -> ([on], [on] -> on)


playDefault :: a -> ([b], [b] -> a)
playDefault x = ([], \[] -> x)


playOne :: (a -> b) -> a -> ([a], [a] -> b)
playOne part item = ([item], \[item] -> part item)

playTwo :: (a -> a -> b) -> a -> a -> ([a], [a] -> b)
playTwo part i1 i2 = ([i1,i2], \[i1,i2] -> part i1 i2)



-- THE PLAYERS

mapUnder :: Play on => (on -> on) -> on -> on
mapUnder f x = f $ generate $ map (mapUnder f) current
    where (current, generate) = replaceChildren x


mapUnderM :: (Monad m, Play on) => (on -> m on) -> on -> m on
mapUnderM f x = mapM (mapUnderM f) current >>= f . generate
    where (current, generate) = replaceChildren x


mapOver :: Play on => (on -> on) -> on -> on
mapOver f x = generate $ map (mapOver f) current
    where (current, generate) = replaceChildren $ f x


mapOverM :: (Monad m, Play on) => (on -> m on) -> on -> m on
mapOverM f x = do
        x2 <- f x
        let (current, generate) = replaceChildren x2
        current2 <- mapM (mapOverM f) current
        return $ generate current2


allOver :: Play on => on -> [on]
allOver x = x : concatMap allOver (fst $ replaceChildren x)


fold :: Play on => ([res] -> tmp) -> (on -> tmp -> res) -> on -> res
fold merge gen x = gen x $ merge $ map (fold merge gen) current
    where current = fst $ replaceChildren x


composM :: (Monad m, Play on) => (on -> m on) -> on -> m on
composM f x = liftM generate $ mapM f current
    where (current, generate) = replaceChildren x

