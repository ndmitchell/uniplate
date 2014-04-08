
import Neil

main :: IO ()
main = do
    retry 3 $ cmd "cabal install haskell-src-exts"
    cmd "runhaskell Main test"
    cmd "ghc --make -O2 Main.hs -o uniplate"
    cmd "./uniplate benchmark"
