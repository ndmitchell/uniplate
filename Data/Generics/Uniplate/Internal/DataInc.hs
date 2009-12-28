import Data.Generics.Uniplate.Internal.Data
import Data.Data

instance (Data a, Typeable a) => Uniplate a where
    uniplate = uniplateData $ fromOracle answer
        where
            answer :: Oracle a
            answer = hitTest (undefined :: a) (undefined :: a)


instance (Data a, Data b, Uniplate b, Typeable a, Typeable b) => Biplate a b where
    biplate = biplateData $ fromOracle answer
        where
            answer :: Oracle b
            answer = hitTest (undefined :: a) (undefined :: b)
