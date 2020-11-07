{-# OPTIONS_GHC -fno-warn-deprecations #-}
{- |
/DEPRECATED/: Use "Data.Generics.Uniplate.Operations" instead.

This module retained Haskell 98 compatability, but users who are happy with
multi-parameter type classes should look towards "Data.Generics.Biplate".

The only function missing from "Data.Generics.Uniplate" is 'fold', as it can be
constructed from 'children' and has little meaning in a multi-typed setting.

All operations, apart from 'childrenOn' should perform identically to their non @On@
counterparts.
-}

module Data.Generics.UniplateOn
    {- DEPRECATED "Use Data.Generics.Uniplate.Operations instead" -}
    (
    module Data.Generics.Uniplate,
    module Data.Generics.UniplateOn
    ) where

import Data.Generics.Uniplate
import Control.Monad(liftM)

-- * Types

-- | Return all the top most children of type @to@ within @from@.
--
-- If @from == to@ then this function should return the root as the single
-- child.
type BiplateType from to = from -> ([to], [to] -> from)


-- * Operations

-- ** Queries

universeOn :: Uniplate to => BiplateType from to -> from -> [to]
universeOn biplate x = concatMap universe $ fst $ biplate x


-- | Return the children of a type. If @to == from@ then it returns the
-- original element (in contrast to 'children')
childrenOn :: Uniplate to => BiplateType from to -> from -> [to]
childrenOn biplate x = fst $ biplate x


-- ** Transformations

transformOn :: Uniplate to => BiplateType from to -> (to -> to) -> from -> from
transformOn biplate f x = generate $ map (transform f) current
    where (current, generate) = biplate x


transformOnM :: (Monad m, Uniplate to) => BiplateType from to -> (to -> m to) -> from -> m from
transformOnM biplate f x = liftM generate $ mapM (transformM f) current
    where (current, generate) = biplate x


rewriteOn :: Uniplate to => BiplateType from to -> (to -> Maybe to) -> from -> from
rewriteOn biplate f x = generate $ map (rewrite f) current
    where (current, generate) = biplate x


rewriteOnM :: (Monad m, Uniplate to) => BiplateType from to -> (to -> m (Maybe to)) -> from -> m from
rewriteOnM biplate f x = liftM generate $ mapM (rewriteM f) current
    where (current, generate) = biplate x


descendOn :: Uniplate to => BiplateType from to -> (to -> to) -> from -> from
descendOn biplate f x = generate $ map f current
    where (current, generate) = biplate x


descendOnM :: (Monad m, Uniplate to) => BiplateType from to -> (to -> m to) -> from -> m from
descendOnM biplate f x = liftM generate $ mapM f current
    where (current, generate) = biplate x


-- ** Other


holesOn :: Uniplate to => BiplateType from to -> from -> [(to, to -> from)]
holesOn biplate x = uncurry f (biplate x)
  where f [] _ = []
        f (x:xs) gen = (x, gen . (:xs)) :
                       f xs (gen . (x:))


contextsOn :: Uniplate to => BiplateType from to -> from -> [(to, to -> from)]
contextsOn biplate x = f (holesOn biplate x)
    where
       f xs = [ (y, ctx . context)
              | (child, ctx) <- xs
              , (y, context) <- contexts child]


-- * Helper for writing instances


-- | Used for defining instances @UniplateFoo a => UniplateFoo [a]@
uniplateOnList :: BiplateType a b -> BiplateType [a] b
uniplateOnList f [] = ([], \[] -> [])
uniplateOnList f (x:xs) =
        (a ++ as,
        \ns -> let (n1,n2) = splitAt (length a) ns in b n1 : bs n2)
    where
        (a , b ) = f x
        (as, bs) = uniplateOnList f xs
