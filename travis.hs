
import System.Process.Extra
import Control.Monad
import Control.Exception.Extra

main :: IO ()
-- These tests no longer work on cabal3, since you can't install a package globally
main = when False $ do
    retry 3 $ system_ "cabal install haskell-src-exts"
    system_ "runhaskell Main test"
    system_ "ghc --make -O2 Main.hs -o uniplate"
    system_ "./uniplate benchmark"
