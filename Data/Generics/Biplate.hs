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
    replaceType :: ReplaceType from to


-- * The Operations

-- ** Queries

universeEx :: Biplate from to => from -> [to]
universeEx = universeOn replaceType


childrenEx :: Biplate from to => from -> [to]
childrenEx = childrenOn replaceType


-- ** Transformations

transformEx :: Biplate from to => (to -> to) -> from -> from
transformEx = transformOn replaceType


transformExM :: (Monad m, Biplate from to) => (to -> m to) -> from -> m from
transformExM = transformOnM replaceType


rewriteEx :: Biplate from to => (to -> Maybe to) -> from -> from
rewriteEx = rewriteOn replaceType


rewriteExM :: (Monad m, Biplate from to) => (to -> m (Maybe to)) -> from -> m from
rewriteExM = rewriteOnM replaceType


descendEx :: Biplate from to => (to -> to) -> from -> from
descendEx = descendOn replaceType


descendExM :: (Monad m, Biplate from to) => (to -> m to) -> from -> m from
descendExM = descendOnM replaceType


-- ** Others

contextsEx:: Biplate from to => from -> [(to, to -> from)]
contextsEx = contextsOn replaceType
