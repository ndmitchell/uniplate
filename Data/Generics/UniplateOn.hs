
module Data.Generics.UniplateOn(
    module Data.Generics.Uniplate,
    module Data.Generics.UniplateOn
    ) where

import Data.Generics.Uniplate
import Control.Monad(liftM)
import Data.List(inits,tails)


type ReplaceType from to = from -> ([to], [to] -> from)


traverseOn :: Play to => ReplaceType from to -> (to -> to) -> from -> from
traverseOn replaceType f x = generate $ map (traverse f) current
    where (current, generate) = replaceType x


traverseOnM :: (Monad m, Play to) => ReplaceType from to -> (to -> m to) -> from -> m from
traverseOnM replaceType f x = liftM generate $ mapM (traverseM f) current
    where (current, generate) = replaceType x


rewriteOn :: Play to => ReplaceType from to -> (to -> Maybe to) -> from -> from
rewriteOn replaceType f x = generate $ map (rewrite f) current
    where (current, generate) = replaceType x


rewriteOnM :: (Monad m, Play to) => ReplaceType from to -> (to -> m (Maybe to)) -> from -> m from
rewriteOnM replaceType f x = liftM generate $ mapM (rewriteM f) current
    where (current, generate) = replaceType x


descendOn :: Play to => ReplaceType from to -> (to -> to) -> from -> from
descendOn replaceType f x = generate $ map (descend f) current
    where (current, generate) = replaceType x


descendOnM :: (Monad m, Play to) => ReplaceType from to -> (to -> m to) -> from -> m from
descendOnM replaceType f x = liftM generate $ mapM (descendM f) current
    where (current, generate) = replaceType x


childrenOn :: Play to => ReplaceType from to -> from -> [to]
childrenOn replaceType x = fst $ replaceType x


everythingOn :: Play to => ReplaceType from to -> from -> [to]
everythingOn replaceType x = concatMap everything $ fst $ replaceType x


everythingContextOn :: Play to => ReplaceType from to -> from -> [(to, to -> from)]
everythingContextOn replaceType x =
        concat [f pre b post | (pre,b:post) <- zip (inits current) (tails current)]
    where
        (current, generate) = replaceType x
        
        f pre x post = [(cur, \new -> generate (pre ++ [new] ++ post))
                       | (cur,gen) <- everythingContext x]


-- foldOn is not defined, does not have sensible semantics


-- useful for definitions
replaceOnList :: ReplaceType a b -> ReplaceType [a] b
replaceOnList f [] = ([], \[] -> [])
replaceOnList f (x:xs) =
        (a ++ as,
        \ns -> let (n1,n2) = splitAt (length a) ns in b n1 : bs n2)
    where
        (a , b ) = f x
        (as, bs) = replaceOnList f xs

