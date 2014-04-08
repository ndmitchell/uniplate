{-|
    SYB compatibility layer. This module serves as a drop-in
    replacement in some situations for some of the SYB operations.
    Users should also import "Data.Generics.Uniplate.Data".

    SYB is described in the paper: \"Scrap your boilerplate: a practical design
    pattern for generic programming\" by Ralf Lammel and Simon
    Peyton Jones.

    * <http://www.cs.vu.nl/boilerplate/>

    * <http://doi.acm.org/10.1145/604174.604179>

    * <http://www.cs.vu.nl/boilerplate/tldi03.pdf>
-}

module Data.Generics.SYB where

import Control.Applicative
import Data.Generics.Uniplate.Operations


-- | @gmapT == 'descend'@
gmapT :: Uniplate a => (a -> a) -> a -> a
gmapT = descend


-- | Use 'children' and 'foldl'
gmapQl :: Uniplate a => (r -> r' -> r) -> r -> (a -> r') -> a -> r
gmapQl combine zero op = foldl combine zero . map op . children


-- | Use 'children' and 'foldr'
gmapQr :: Uniplate a => (r' -> r -> r) -> r -> (a -> r') -> a -> r
gmapQr combine zero op = foldr combine zero . map op . children


-- | Use 'children'
gmapQ :: Uniplate a => (a -> u) -> a -> [u]
gmapQ f = map f . children


-- | Use 'children' and '!!'
gmapQi :: Uniplate a => Int -> (a -> u) -> a -> u
gmapQi i f x = gmapQ f x !! i


-- | @gmapM == 'descendM'@
gmapM :: (Uniplate a, Applicative m) => (a -> m a) -> a -> m a
gmapM = descendM



-- | @mkT == 'id'@
mkT :: (a -> a) -> (a -> a)
mkT = id


-- | @everywhere == 'transformBi'@
everywhere :: Biplate b a => (a -> a) -> b -> b
everywhere = transformBi


-- | @mkM == id@
mkM :: (a -> m a) -> a -> m a
mkM = id


-- | @everywhereM == 'transformBiM'@
everywhereM :: (Biplate b a, Monad m, Applicative m) => (a -> m a) -> b -> m b
everywhereM = transformBiM



-- | Only for use with 'everything'
mkQ :: r -> (a -> r) -> (r, a -> r)
mkQ = (,)


-- | Use 'universe' or 'universeBi', perhaps followed by a fold.
--
--   Not an exact equivalent to the SYB @everything@, as the
--   operators may be applied in different orders.
everything :: Biplate b a => (r -> r -> r) -> (r, a -> r) -> b -> r
everything combine (nil, op) = foldl combine nil . map op . universeBi
