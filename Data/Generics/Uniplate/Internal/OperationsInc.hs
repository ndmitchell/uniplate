import Control.Applicative
import Control.Arrow
import Data.Generics.Uniplate.Internal.Utils
import Data.Monoid


newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity $ f x


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
    uniplate :: Applicative m => on -> (on -> m on) -> m on

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
    descend f x = runIdentity $ descendM (pure . f) x

    -- | Applicative variant of 'descend'
    {-# INLINE descendM #-}
    descendM :: Applicative m => (on -> m on) -> on -> m on
    descendM = flip uniplate



-- | Children are defined as the top-most items of type to
--   /starting at the root/. All instances must define 'biplate', while
--   'descendBi' and 'descendBiM' are optional.
class Uniplate to => Biplate from to where
    -- | Return all the top most children of type @to@ within @from@.
    --
    --   If @from == to@ then this function should return the root as the single
    --   child.
    biplate :: Applicative m => from -> (to -> m to) -> m from


    -- | Like 'descend' but with more general types. If @from == to@ then this
    --   function /does not/ descend. Therefore, when writing definitions it is
    --   highly unlikely that this function should be used in the recursive case.
    --   A common pattern is to first match the types using 'descendBi', then continue
    --   the recursion with 'descend'.
    {-# INLINE descendBi #-}
    descendBi :: (to -> to) -> from -> from
    descendBi f x = runIdentity $ descendBiM (pure . f) x

    {-# INLINE descendBiM #-}
    descendBiM :: Applicative m => (to -> m to) -> from -> m from
    descendBiM = flip biplate


newtype Collect a b = Collect {collected :: a}

instance Functor (Collect a) where
    fmap f (Collect a) = Collect a

instance Monoid a => Applicative (Collect a) where
    pure x = Collect mempty
    Collect a <*> Collect b = Collect (a `mappend` b)


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
            where g x = Collect (Endo (cons x)) <*> descendM g x


-- | Get the direct children of a node. Usually using 'universe' is more appropriate.
children :: Uniplate on => on -> [on]
children x = builder $ \cons nil -> ($ nil) $ appEndo $ collected $ descendM (\x -> Collect $ Endo $ cons x) x


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



data Replace v a = Replace {replaced :: [(v, v -> a)], replacedValue :: a}

instance Functor (Replace v) where
    fmap f (Replace xs v) = Replace (map (second (f .)) xs) (f v)

instance Applicative (Replace v) where
    pure v = Replace [] v
    Replace xs1 f <*> Replace xs2 v = Replace (ys1 ++ ys2) (f v)
        where ys1 = map (second (($ v) .)) xs1
              ys2 = map (second (f .)) xs2


-- | The one depth version of 'contexts'
--
-- > children x == map fst (holes x)
-- > all (== x) [b a | (a,b) <- holes x]
holes :: Uniplate on => on -> [(on, on -> on)]
holes x = replaced $ descendM (\v -> Replace [(v, id)] v) x

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
            where g x = Collect (Endo (cons x)) <*> descendM g x

-- | Return the children of a type. If @to == from@ then it returns the
-- original element (in contrast to 'children')
childrenBi :: Biplate from to => from -> [to]
childrenBi x = builder $ \cons nil -> ($ nil) $ appEndo $ collected $ descendBiM (\x -> Collect (Endo $ cons x)) x


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

contextsBi :: Biplate from to => from -> [(to, to -> from)]
contextsBi = f . holesBi
    where
       f xs = [ (y, ctx . context)
              | (child, ctx) <- xs
              , (y, context) <- contexts child]


holesBi :: Biplate from to => from -> [(to, to -> from)]
holesBi x = replaced $ descendBiM (\v -> Replace [(v, id)] v) x
