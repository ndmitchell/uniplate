import Data.Generics.Uniplate.Internal.Data
import Data.Data

instance (Data a, Typeable a) => Uniplate a where
    uniplate = collect_generate (fromBox answer)
        where
            answer :: Box a
            answer = containsMatch (undefined :: a) (undefined :: a)


instance (Data a, Data b, Uniplate b, Typeable a, Typeable b) => Biplate a b where
    biplate = collect_generate_self (fromBox answer)
        where
            answer :: Box b
            answer = containsMatch (undefined :: a) (undefined :: b)
