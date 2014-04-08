
import Neil

main :: IO ()
main = do
    cmd "runhaskell Main test"
    cmd "ghc --make -O2 Main.hs -o uniplate"
    cmd "./uniplate benchmark"
