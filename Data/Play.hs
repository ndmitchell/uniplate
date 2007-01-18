
module Data.Play where

import Control.Monad
import Data.List(inits,tails)


-- THE CLASS
class Play on where
    replaceChildren :: on -> ([on], [on] -> on)
    
    getChildren :: on -> [on]
    getChildren = fst . replaceChildren


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
mapOverM f x = do (current, generate) <- liftM replaceChildren $ f x
                  liftM generate $ mapM (mapOverM f) current


allOver :: Play on => on -> [on]
allOver x = allOverRest x []
    where
        allOverRest :: Play on => on -> [on] -> [on]
        allOverRest x rest = x : concat2 (map allOverRest $ getChildren x) rest
        
        concat2 :: [[a] -> [a]] -> [a] -> [a]
        concat2 [] rest = rest
        concat2 (x:xs) rest = x (concat2 xs rest)


allOverContext :: Play on => on -> [(on, on -> on)]
allOverContext x = (x,id) : f current
  where
    (current, generate) = replaceChildren x
    f xs = [ (y, \i -> generate (pre ++ [context i] ++ post))
           | (pre,b:post) <- zip (inits xs) (tails xs)
           , (y, context) <- allOverContext b]


fold :: Play on => ([res] -> tmp) -> (on -> tmp -> res) -> on -> res
fold merge gen x = gen x $ merge $ map (fold merge gen) $ getChildren x


compos :: Play on => (on -> on) -> on -> on
compos f x = generate $ map f current
    where (current, generate) = replaceChildren x

    
composM :: (Monad m, Play on) => (on -> m on) -> on -> m on
composM f x = liftM generate $ mapM f current
    where (current, generate) = replaceChildren x

