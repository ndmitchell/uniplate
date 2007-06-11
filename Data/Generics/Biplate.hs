{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
    Requires multi-parameter type classes, so is no longer Haskell 98. These operations
    are easier to use and construct than the equivalent @Data.Generics.UniplateOn@ methods,
    but perform the same operation.
    
    It is recommended that instead of importing this module, you import one of the following
    modules, to construct instances:
    
    * "Data.Generics.PlateDirect" - does not require overlapping instances, highest performance
    but requires /O(n^2)/ instances in the worst case.
    
    * "Data.Generics.PlateTypeable" - requires the "Data.Typeable" class for all data structures.
    
    * "Data.Generics.PlateData" - requires "Data.Generics" and the 'Data' class, which is only
    available on GHC, but automatically infers instances.
-}

module Data.Generics.Biplate(
    module Data.Generics.UniplateOn,
    module Data.Generics.Biplate
    ) where

import Data.Generics.UniplateOn


-- * The Class

-- | Children are defined as the top-most items of type to
--   /starting at the root/.
class Uniplate to => Biplate from to where
    biplate :: BiplateType from to


-- * The Operations

-- ** Queries

universeEx :: Biplate from to => from -> [to]
universeEx = universeOn biplate


childrenEx :: Biplate from to => from -> [to]
childrenEx = childrenOn biplate


-- ** Transformations

transformEx :: Biplate from to => (to -> to) -> from -> from
transformEx = transformOn biplate


transformExM :: (Monad m, Biplate from to) => (to -> m to) -> from -> m from
transformExM = transformOnM biplate


rewriteEx :: Biplate from to => (to -> Maybe to) -> from -> from
rewriteEx = rewriteOn biplate


rewriteExM :: (Monad m, Biplate from to) => (to -> m (Maybe to)) -> from -> m from
rewriteExM = rewriteOnM biplate


descendEx :: Biplate from to => (to -> to) -> from -> from
descendEx = descendOn biplate


descendExM :: (Monad m, Biplate from to) => (to -> m to) -> from -> m from
descendExM = descendOnM biplate


-- ** Others

contextsEx:: Biplate from to => from -> [(to, to -> from)]
contextsEx = contextsOn biplate
