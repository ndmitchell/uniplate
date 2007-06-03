
module Data.Generics.Uniplate where

import Control.Monad
import Data.List(inits,tails)
import Data.Generics.PlateInternal


-- * The Class

type ReplaceChildren on = on -> ([on], [on] -> on)

class Play on where
    replaceChildren :: ReplaceChildren on
    
children :: Play on => on -> [on]
children = fst . replaceChildren


-- * The Operations

transform :: Play on => (on -> on) -> on -> on
transform f x = f $ generate $ map (transform f) current
    where (current, generate) = replaceChildren x


transformM :: (Monad m, Play on) => (on -> m on) -> on -> m on
transformM f x = mapM (transformM f) current >>= f . generate
    where (current, generate) = replaceChildren x


rewrite :: Play on => (on -> Maybe on) -> on -> on
rewrite f = transform g
    where g x = maybe x (rewrite f) (f x)


rewriteM :: (Monad m, Play on) => (on -> m (Maybe on)) -> on -> m on
rewriteM f = transformM g
    where g x = f x >>= maybe (return x) (rewriteM f)


descend :: Play on => (on -> on) -> on -> on
descend f x = generate $ map f current
    where (current, generate) = replaceChildren x

    
descendM :: (Monad m, Play on) => (on -> m on) -> on -> m on
descendM f x = liftM generate $ mapM f current
    where (current, generate) = replaceChildren x


universe :: Play on => on -> [on]
universe x = builder (f x)
    where
        f :: Play on => on -> (on -> res -> res) -> res -> res
        f x cons nil = x `cons` concatCont (map (\x -> f x cons) $ children x) nil


contexts :: Play on => on -> [(on, on -> on)]
contexts x = (x,id) : f current
  where
    (current, generate) = replaceChildren x
    f xs = [ (y, \i -> generate (pre ++ [context i] ++ post))
           | (pre,b:post) <- zip (inits xs) (tails xs)
           , (y, context) <- contexts b]


fold :: Play on => (on -> [r] -> r) -> on -> r
fold op x = op x $ map (fold op) $ children x

