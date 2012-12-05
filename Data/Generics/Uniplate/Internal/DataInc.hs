import Data.Generics.Uniplate.Internal.Data
import Data.Data

instance Data a => Uniplate a where
    uniplate = uniplateData $ fromOracle answer
        where answer = hitTest (undefined :: a) (undefined :: a)

    descend = descendData $ fromOracle answer
        where answer = hitTest (undefined :: a) (undefined :: a)

    descendM = descendDataM $ fromOracle answer
        where answer = hitTest (undefined :: a) (undefined :: a)

instance (Data a, Data b, Uniplate b) => Biplate a b where
    biplate = biplateData $ fromOracle answer
        where answer = hitTest (undefined :: a) (undefined :: b)

    descendBi = descendBiData $ fromOracle answer
        where answer = hitTest (undefined :: a) (undefined :: b)

    descendBiM = descendBiDataM $ fromOracle answer
        where answer = hitTest (undefined :: a) (undefined :: b)
