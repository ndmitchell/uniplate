
import System.Process.Extra
import Control.Exception.Extra

main :: IO ()
main = do
    retry 3 $ system_ "cabal install haskell-src-exts"
    system_ "runhaskell Main test"
    system_ "ghc --make -O2 Main.hs -o uniplate"
    system_ "./uniplate benchmark"
