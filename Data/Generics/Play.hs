
module Data.Generics.Play where

import Control.Monad
import Data.List(inits,tails)
import Data.Generics.PlayInternal


-- * The Class

type ReplaceChildren on = on -> ([on], [on] -> on)

class Play on where
    replaceChildren :: ReplaceChildren on
    
    getChildren :: on -> [on]
    getChildren = fst . replaceChildren


-- * The Operations

traverse :: Play on => (on -> on) -> on -> on
traverse f x = f $ generate $ map (traverse f) current
    where (current, generate) = replaceChildren x


traverseM :: (Monad m, Play on) => (on -> m on) -> on -> m on
traverseM f x = mapM (traverseM f) current >>= f . generate
    where (current, generate) = replaceChildren x


rewrite :: Play on => (on -> Maybe on) -> on -> on
rewrite f = traverse g
    where g x = maybe x (rewrite f) (f x)


rewriteM :: (Monad m, Play on) => (on -> m (Maybe on)) -> on -> m on
rewriteM f = traverseM g
    where g x = f x >>= maybe (return x) (rewriteM f)


descend :: Play on => (on -> on) -> on -> on
descend f x = generate $ map f current
    where (current, generate) = replaceChildren x

    
descendM :: (Monad m, Play on) => (on -> m on) -> on -> m on
descendM f x = liftM generate $ mapM f current
    where (current, generate) = replaceChildren x


everything :: Play on => on -> [on]
everything x = builder (f x)
    where
        f :: Play on => on -> (on -> res -> res) -> res -> res
        f x cons nil = x `cons` concatCont (map (\x -> f x cons) $ getChildren x) nil


everythingContext :: Play on => on -> [(on, on -> on)]
everythingContext x = (x,id) : f current
  where
    (current, generate) = replaceChildren x
    f xs = [ (y, \i -> generate (pre ++ [context i] ++ post))
           | (pre,b:post) <- zip (inits xs) (tails xs)
           , (y, context) <- everythingContext b]


fold :: Play on => ([res] -> tmp) -> (on -> tmp -> res) -> on -> res
fold merge gen x = gen x $ merge $ map (fold merge gen) $ getChildren x

