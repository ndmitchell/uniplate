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
    module Data.Generics.UniplateStrOn,
    module Data.Generics.Biplate
    ) where

import Data.Generics.UniplateStrOn


-- * The Class

-- | Children are defined as the top-most items of type to
--   /starting at the root/.
class Uniplate to => Biplate from to where
    biplate :: BiplateType from to


-- * The Operations

-- ** Queries

universeBi :: Biplate from to => from -> [to]
universeBi = universeOn biplate


childrenBi :: Biplate from to => from -> [to]
childrenBi = childrenOn biplate


-- ** Transformations

transformBi :: Biplate from to => (to -> to) -> from -> from
transformBi = transformOn biplate


transformBiM :: (Monad m, Biplate from to) => (to -> m to) -> from -> m from
transformBiM = transformOnM biplate


rewriteBi :: Biplate from to => (to -> Maybe to) -> from -> from
rewriteBi = rewriteOn biplate


rewriteBiM :: (Monad m, Biplate from to) => (to -> m (Maybe to)) -> from -> m from
rewriteBiM = rewriteOnM biplate


descendBi :: Biplate from to => (to -> to) -> from -> from
descendBi = descendOn biplate


descendBiM :: (Monad m, Biplate from to) => (to -> m to) -> from -> m from
descendBiM = descendOnM biplate


-- ** Others

contextsBi:: Biplate from to => from -> [(to, to -> from)]
contextsBi = contextsOn biplate


holesBi:: Biplate from to => from -> [(to, to -> from)]
holesBi = holesOn biplate
