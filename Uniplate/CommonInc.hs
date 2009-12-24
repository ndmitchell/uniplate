
import Uniplate.Type

test :: IO ()
test = if prop1 then return () else error "failed"

prop1 = universe (Add (Val 1) (Neg (Val 2))) == [Add (Val 1) (Neg (Val 2)), Val 1, Neg (Val 2), Val 2]
