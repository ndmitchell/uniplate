{- |
This is the main Uniplate module, which defines all the essential operations
in a Haskell 98 compatible manner.

Most functions have an example of a possible use for the function.
To illustate, I have used the @Expr@ type as below:

> data Expr = Val Int
>           | Neg Expr
>           | Add Expr Expr
-}


module Data.Generics.Uniplate where

import Control.Monad hiding (mapM)
import Data.List(inits,tails)
import Control.Monad.State hiding (mapM)
import Data.Traversable
import Prelude hiding (mapM)

import Data.Generics.PlateInternal
import Data.Generics.Str


-- * The Class

-- | The type of replacing all the children of a node
--
--   Taking a value, the function should return all the immediate children
--   of the same type, and a function to replace them.
type UniplateType on = on -> (Str on, Str on -> on)

-- | The standard Uniplate class, all operations require this.
--   Must define one of 'uniplate' or 'uniplateStr', 'uniplateStr' is recommended
class Uniplate on where
    -- | The underlying method in the class
    --
    -- > uniplateStr (Add (Val 1) (Neg (Val 2)))
    -- >    = (Two (One (Val 1)) (One (Neg (Val 2)))], \(Two (One a) (One b)) -> Add a b)
    -- > uniplateStr (Val 1)
    -- >    = (Zero                                  , \Zero                  -> Val 1  )
    uniplateStr :: UniplateType on
    uniplateStr x = (strList spine, gen . listStr)
      where (spine, gen) = uniplate x

    -- | The list version of the method
    --
    -- > uniplate (Add (Val 1) (Neg (Val 2))) = ([Val 1, Neg (Val 2)], \[a,b] -> Add a b)
    -- > uniplate (Val 1)                     = ([]                  , \[]    -> Val 1  )
    uniplate :: on -> ([on], [on] -> on)
    uniplate x = (g spine [], \children -> gen (evalState (mapM fill spine) children))
      where (spine, gen) = uniplateStr x
            g Zero rest = rest
            g (One x) rest = x:rest
            g (Two l r) rest = g l (g r rest)
            fill _ = do (x:rest) <- get; put rest; return x

-- * The Operations

-- ** Queries

-- | Get all the children of a node, including itself and all children.
--
-- > universe (Add (Val 1) (Neg (Val 2))) =
-- >     [Add (Val 1) (Neg (Val 2)), Val 1, Neg (Val 2), Val 2]
--
-- This method is often combined with a list comprehension, for example:
--
-- > vals x = [Val i | i <- universe x]
universe :: Uniplate on => on -> [on]
universe x = builder f
    where
        f cons nil = g cons nil (One x) nil
        g cons nil Zero res = res
        g cons nil (One x) res = x `cons` g cons nil (fst $ uniplateStr x) res
        g cons nil (Two x y) res = g cons nil x (g cons nil y res)



-- | Get the direct children of a node. Usually using 'universe' is more appropriate.
--
-- @children = fst . 'uniplate'@
children :: Uniplate on => on -> [on]
children x = builder f
    where
        f cons nil = g cons nil (fst $ uniplateStr x) nil
        g cons nil Zero res = res
        g cons nil (One x) res = x `cons` res
        g cons nil (Two x y) res = g cons nil x (g cons nil y res)


-- ** Transformations


-- | Transform every element in the tree, in a bottom-up manner.
--
-- For example, replacing negative literals with literals:
--
-- > negLits = trasform f
-- >    where f (Neg (Lit i)) = Lit (negate i)
-- >          f x = x
transform :: Uniplate on => (on -> on) -> on -> on
transform f = f . descend (transform f)


-- | Monadic variant of 'transform'
transformM :: (Monad m, Uniplate on) => (on -> m on) -> on -> m on
transformM f x = f =<< descendM (transformM f) x


-- | Rewrite by applying a rule everywhere you can. Ensures that the rule cannot
-- be applied anywhere in the result:
--
-- > propRewrite r x = all (isNothing . r) (universe (rewrite r x))
--
-- Usually 'transform' is more appropriate, but 'rewrite' can give better
-- compositionality. Given two single transformations @f@ and @g@, you can
-- construct @f `mplus` g@ which performs both rewrites until a fixed point.
rewrite :: Uniplate on => (on -> Maybe on) -> on -> on
rewrite f = transform g
    where g x = maybe x (rewrite f) (f x)


-- | Monadic variant of 'rewrite'
rewriteM :: (Monad m, Uniplate on) => (on -> m (Maybe on)) -> on -> m on
rewriteM f = transformM g
    where g x = f x >>= maybe (return x) (rewriteM f)


-- | Perform a transformation on all the immediate children, then combine them back.
-- This operation allows additional information to be passed downwards, and can be
-- used to provide a top-down transformation.
descend :: Uniplate on => (on -> on) -> on -> on
descend f x = generate $ fmap f current
    where (current, generate) = uniplateStr x


-- | Monadic variant of 'descend'    
descendM :: (Monad m, Uniplate on) => (on -> m on) -> on -> m on
descendM f x = liftM generate $ mapM f current
    where (current, generate) = uniplateStr x

-- ** Others

-- | Return all the contexts and holes. This operation is only occasionally useful.
--
-- > propUniverse x = universe x == map fst (contexts x)
-- > propId x = all (== x) [b a | (a,b) <- contexts x]
contexts :: Uniplate on => on -> [(on, on -> on)]
contexts x = (x,id) : f (holes x)
  where
    f xs = [ (y, ctx . context)
           | (child, ctx) <- xs
           , (y, context) <- contexts child]

-- | The one depth version of 'contexts'
holes :: Uniplate on => on -> [(on, on -> on)]
holes x = uncurry f (uniplateStr x)
  where f Zero _ = []
        f (One i) generate = [(i, generate . One)]
        f (Two l r) gen = f l (gen . (\i -> Two i r))
                       ++ f r (gen . (\i -> Two l i))

-- | Perform a fold-like computation on each value,
--   technically a paramorphism
para :: Uniplate on => (on -> [r] -> r) -> on -> r
para op x = op x $ map (para op) $ children x
