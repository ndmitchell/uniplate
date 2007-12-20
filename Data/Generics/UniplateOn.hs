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
import Data.Generics.PlateInternal

-- * Types

-- | Return all the top most children of type @to@ within @from@.
--
-- If @from == to@ then this function should return the root as the single
-- child.
type BiplateType from to = from -> (Str to, Str to -> from)


-- * Operations

-- ** Queries

universeOn :: Uniplate to => BiplateType from to -> from -> [to]
universeOn biplate x = builder f
    where
        f cons nil = g cons nil (fst $ biplate x) nil
        g cons nil Zero res = res
        g cons nil (One x) res = x `cons` g cons nil (fst $ uniplate x) res
        g cons nil (Two x y) res = g cons nil x (g cons nil y res)


-- | Return the children of a type. If @to == from@ then it returns the
-- original element (in constract to 'children'
childrenOn :: Uniplate to => BiplateType from to -> from -> [to]
childrenOn biplate x = builder f
    where
        f cons nil = g cons nil (fst $ biplate x) nil
        g cons nil Zero res = res
        g cons nil (One x) res = x `cons` res
        g cons nil (Two x y) res = g cons nil x (g cons nil y res)


-- ** Transformations

transformOn :: Uniplate to => BiplateType from to -> (to -> to) -> from -> from
transformOn biplate f x = generate $ pam (transform f) current
    where (current, generate) = biplate x


transformOnM :: (Monad m, Uniplate to) => BiplateType from to -> (to -> m to) -> from -> m from
transformOnM biplate f x = liftM generate $ pamM (transformM f) current
    where (current, generate) = biplate x


rewriteOn :: Uniplate to => BiplateType from to -> (to -> Maybe to) -> from -> from
rewriteOn biplate f x = generate $ pam (rewrite f) current
    where (current, generate) = biplate x


rewriteOnM :: (Monad m, Uniplate to) => BiplateType from to -> (to -> m (Maybe to)) -> from -> m from
rewriteOnM biplate f x = liftM generate $ pamM (rewriteM f) current
    where (current, generate) = biplate x


descendOn :: Uniplate to => BiplateType from to -> (to -> to) -> from -> from
descendOn biplate f x = generate $ pam f current
    where (current, generate) = biplate x


descendOnM :: (Monad m, Uniplate to) => BiplateType from to -> (to -> m to) -> from -> m from
descendOnM biplate f x = liftM generate $ pamM f current
    where (current, generate) = biplate x


{-
-- ** Other

contextsOn :: Uniplate to => BiplateType from to -> from -> [(to, to -> from)]
contextsOn biplate x =
        concat [f pre b post | (pre,b:post) <- zip (inits current) (tails current)]
    where
        (current, generate) = biplate x
        
        f pre x post = [(cur, \new -> generate (pre ++ [new] ++ post))
                       | (cur,gen) <- contexts x]


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

-}
