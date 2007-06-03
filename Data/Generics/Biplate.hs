{-# OPTIONS_GHC -fglasgow-exts #-}

module Data.Generics.Biplate(
    module Data.Generics.UniplateOn,
    module Data.Generics.Biplate
    ) where

import Data.Generics.UniplateOn


-- * The Class

-- | Children are defined as the top-most items of type to
--   /starting at the root/.
class Play to => PlayEx from to where
    replaceType :: ReplaceType from to


-- * The Operations

traverseEx :: PlayEx from to => (to -> to) -> from -> from
traverseEx = traverseOn replaceType


traverseExM :: (Monad m, PlayEx from to) => (to -> m to) -> from -> m from
traverseExM = traverseOnM replaceType


rewriteEx :: PlayEx from to => (to -> Maybe to) -> from -> from
rewriteEx = rewriteOn replaceType


rewriteExM :: (Monad m, PlayEx from to) => (to -> m (Maybe to)) -> from -> m from
rewriteExM = rewriteOnM replaceType


descendEx :: PlayEx from to => (to -> to) -> from -> from
descendEx = descendOn replaceType


descendExM :: (Monad m, PlayEx from to) => (to -> m to) -> from -> m from
descendExM = descendOnM replaceType


childrenEx :: PlayEx from to => from -> [to]
childrenEx = childrenOn replaceType


everythingEx :: PlayEx from to => from -> [to]
everythingEx = everythingOn replaceType


everythingContextEx :: PlayEx from to => from -> [(to, to -> from)]
everythingContextEx = everythingContextOn replaceType
