import Control.Applicative
import Data.Generics.Str
import Data.Generics.Uniplate.Internal.Utils
import Data.Monoid


-- * The Classes

-- | The standard Uniplate class, all operations require this. All definitions must
--   define 'uniplate', while 'descend' and 'descendM' are optional.
class Uniplate on where
    -- | The underlying method in the class.
    --   Taking a value, the function should return all the immediate children
    --   of the same type, and a function to replace them.
    --
    --   Given @uniplate x = (cs, gen)@
    --
    --   @cs@ should be a @Str on@, constructed of @Zero@, @One@ and @Two@,
    --   containing all @x@'s direct children of the same type as @x@. @gen@
    --   should take a @Str on@ with exactly the same structure as @cs@,
    --   and generate a new element with the children replaced.
    --
    --   Example instance:
    --
    -- > instance Uniplate Expr where
    -- >     uniplate (Val i  ) = (Zero               , \Zero                  -> Val i  )
    -- >     uniplate (Neg a  ) = (One a              , \(One a)               -> Neg a  )
    -- >     uniplate (Add a b) = (Two (One a) (One b), \(Two (One a) (One b)) -> Add a b)
    uniplate :: on -> (Str on, Str on -> on)

    -- | Perform a transformation on all the immediate children, then combine them back.
    --   This operation allows additional information to be passed downwards, and can be
    --   used to provide a top-down transformation. This function can be defined explicitly,
    --   or can be provided by automatically in terms of 'uniplate'.
    --
    --   For example, on the sample type, we could write:
    --
    -- > descend f (Val i  ) = Val i
    -- > descend f (Neg a  ) = Neg (f a)
    -- > descend f (Add a b) = Add (f a) (f b)
    {-# INLINE descend #-}
    descend :: (on -> on) -> on -> on
    descend f x = case uniplate x of
        (current, generate) -> generate $ strMap f current

    -- | Applicative variant of 'descend'
    {-# INLINE descendM #-}
    descendM :: Applicative m => (on -> m on) -> on -> m on
    descendM f x = case uniplate x of
        (current, generate) -> generate <$> strMapM f current



-- | Children are defined as the top-most items of type to
--   /starting at the root/. All instances must define 'biplate', while
--   'descendBi' and 'descendBiM' are optional.
class Uniplate to => Biplate from to where
    -- | Return all the top most children of type @to@ within @from@.
    --
    --   If @from == to@ then this function should return the root as the single
    --   child.
    biplate :: from -> (Str to, Str to -> from)


    -- | Like 'descend' but with more general types. If @from == to@ then this
    --   function /does not/ descend. Therefore, when writing definitions it is
    --   highly unlikely that this function should be used in the recursive case.
    --   A common pattern is to first match the types using 'descendBi', then continue
    --   the recursion with 'descend'.
    {-# INLINE descendBi #-}
    descendBi :: (to -> to) -> from -> from
    descendBi f x = case biplate x of
        (current, generate) -> generate $ strMap f current

    {-# INLINE descendBiM #-}
    descendBiM :: Applicative m => (to -> m to) -> from -> m from
    descendBiM f x = case biplate x of
        (current, generate) -> generate <$> strMapM f current


data Collect a b = Collect {collected :: a, fromCollect :: b}

instance Functor (Collect a) where
    fmap f (Collect a b) = Collect a $ f b

instance Monoid a => Applicative (Collect a) where
    pure = Collect mempty
    Collect a f <*> Collect b x = Collect (a `mappend` b) (f x)


-- * Single Type Operations

-- ** Queries

-- | Get all the children of a node, including itself and all children.
--
-- > universe (Add (Val 1) (Neg (Val 2))) =
-- >     [Add (Val 1) (Neg (Val 2)), Val 1, Neg (Val 2), Val 2]
--
-- This method is often combined with a list comprehension, for example:
--
-- > vals x = [i | Val i <- universe x]
{-# INLINE universe #-}
universe :: Uniplate on => on -> [on]
universe x = builder f
    where
        f cons nil = ($ nil) $ appEndo $ collected $ g x
            where g x = Collect (Endo (cons x)) id <*> descendM g x


-- | Get the direct children of a node. Usually using 'universe' is more appropriate.
children :: Uniplate on => on -> [on]
children x = builder $ \cons nil -> ($ nil) $ appEndo $ collected $ descendM (\x -> Collect (Endo $ cons x) x) x


-- ** Transformations


-- | Transform every element in the tree, in a bottom-up manner.
--
-- For example, replacing negative literals with literals:
--
-- > negLits = transform f
-- >    where f (Neg (Lit i)) = Lit (negate i)
-- >          f x = x
transform :: Uniplate on => (on -> on) -> on -> on
transform f = g
    where g = f . descend g


-- | Applicative variant of 'transform'
transformM :: (Monad m, Applicative m, Uniplate on) => (on -> m on) -> on -> m on
transformM f = g
    where g x = f =<< descendM g x

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


-- | Applicative variant of 'rewrite'
rewriteM :: (Monad m, Applicative m, Uniplate on) => (on -> m (Maybe on)) -> on -> m on
rewriteM f = transformM g
    where g x = f x >>= maybe (return x) (rewriteM f)


-- ** Others

-- | Return all the contexts and holes.
--
-- > universe x == map fst (contexts x)
-- > all (== x) [b a | (a,b) <- contexts x]
contexts :: Uniplate on => on -> [(on, on -> on)]
contexts x = (x,id) : f (holes x)
  where
    f xs = [ (y, ctx . context)
           | (child, ctx) <- xs
           , (y, context) <- contexts child]


-- | The one depth version of 'contexts'
--
-- > children x == map fst (holes x)
-- > all (== x) [b a | (a,b) <- holes x]
holes :: Uniplate on => on -> [(on, on -> on)]
holes x = uncurry f (uniplate x)
  where f Zero _ = []
        f (One i) generate = [(i, generate . One)]
        f (Two l r) gen = f l (gen . (\i -> Two i r))
                       ++ f r (gen . (\i -> Two l i))

-- | Perform a fold-like computation on each value,
--   technically a paramorphism
para :: Uniplate on => (on -> [r] -> r) -> on -> r
para op x = op x $ map (para op) $ children x



-- * Multiple Type Operations

-- ** Queries

{-# INLINE universeBi #-}
universeBi :: Biplate from to => from -> [to]
universeBi x = builder f
    where
        f cons nil = ($ nil) $ appEndo $ collected $ descendBiM g x
            where g x = Collect (Endo (cons x)) id <*> descendM g x

-- | Return the children of a type. If @to == from@ then it returns the
-- original element (in contrast to 'children')
childrenBi :: Biplate from to => from -> [to]
childrenBi x = builder $ \cons nil -> ($ nil) $ appEndo $ collected $ descendBiM (\x -> Collect (Endo $ cons x) x) x


-- ** Transformations

{-# INLINE transformBi #-}
transformBi :: Biplate from to => (to -> to) -> from -> from
transformBi f = descendBi (transform f)


{-# INLINE transformBiM #-}
transformBiM :: (Monad m, Applicative m, Biplate from to) => (to -> m to) -> from -> m from
transformBiM f = descendBiM (transformM f)


rewriteBi :: Biplate from to => (to -> Maybe to) -> from -> from
rewriteBi f = descendBi (rewrite f)


rewriteBiM :: (Monad m, Applicative m, Biplate from to) => (to -> m (Maybe to)) -> from -> m from
rewriteBiM f = descendBiM (rewriteM f)


-- ** Others

contextsBi:: Biplate from to => from -> [(to, to -> from)]
contextsBi = f . holesBi
    where
       f xs = [ (y, ctx . context)
              | (child, ctx) <- xs
              , (y, context) <- contexts child]


holesBi:: Biplate from to => from -> [(to, to -> from)]
holesBi = uncurry f . biplate
  where f Zero _ = []
        f (One i) generate = [(i, generate . One)]
        f (Two l r) gen = f l (gen . (\i -> Two i r))
                       ++ f r (gen . (\i -> Two l i))
