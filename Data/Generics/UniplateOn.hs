{- |
This module retained Haskell 98 compatability, but users who are happy with
multi-parameter type classes should look towards "Data.Generics.Biplate".

The only function missing from "Data.Generics.Uniplate" is 'fold', as it can be
constructed from 'children' and has little meaning in a multi-typed setting.

All operations, apart from 'childrenOn' should perform identically to their non @On@
counterparts.
-}

module Data.Generics.UniplateOn(
    module Data.Generics.Uniplate,
    module Data.Generics.UniplateOn
    ) where

import Data.Generics.Uniplate
import Control.Monad(liftM)
import Data.List(inits,tails)

-- * Types

-- | Return all the top most children of type @to@ within @from@.
--
-- If @from == to@ then this function should return the root as the single
-- child.
type ReplaceType from to = from -> ([to], [to] -> from)


-- * Operations

-- ** Queries

universeOn :: Uniplate to => ReplaceType from to -> from -> [to]
universeOn replaceType x = concatMap universe $ fst $ replaceType x


-- | Return the children of a type. If @to == from@ then it returns the
-- original element (in constract to 'children'
childrenOn :: Uniplate to => ReplaceType from to -> from -> [to]
childrenOn replaceType x = fst $ replaceType x


-- ** Transformations

transformOn :: Uniplate to => ReplaceType from to -> (to -> to) -> from -> from
transformOn replaceType f x = generate $ map (transform f) current
    where (current, generate) = replaceType x


transformOnM :: (Monad m, Uniplate to) => ReplaceType from to -> (to -> m to) -> from -> m from
transformOnM replaceType f x = liftM generate $ mapM (transformM f) current
    where (current, generate) = replaceType x


rewriteOn :: Uniplate to => ReplaceType from to -> (to -> Maybe to) -> from -> from
rewriteOn replaceType f x = generate $ map (rewrite f) current
    where (current, generate) = replaceType x


rewriteOnM :: (Monad m, Uniplate to) => ReplaceType from to -> (to -> m (Maybe to)) -> from -> m from
rewriteOnM replaceType f x = liftM generate $ mapM (rewriteM f) current
    where (current, generate) = replaceType x


descendOn :: Uniplate to => ReplaceType from to -> (to -> to) -> from -> from
descendOn replaceType f x = generate $ map (descend f) current
    where (current, generate) = replaceType x


descendOnM :: (Monad m, Uniplate to) => ReplaceType from to -> (to -> m to) -> from -> m from
descendOnM replaceType f x = liftM generate $ mapM (descendM f) current
    where (current, generate) = replaceType x


-- ** Other

contextsOn :: Uniplate to => ReplaceType from to -> from -> [(to, to -> from)]
contextsOn replaceType x =
        concat [f pre b post | (pre,b:post) <- zip (inits current) (tails current)]
    where
        (current, generate) = replaceType x
        
        f pre x post = [(cur, \new -> generate (pre ++ [new] ++ post))
                       | (cur,gen) <- contexts x]
