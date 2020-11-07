
import System.Process.Extra
import Data.List
import Control.Monad
import Control.Exception.Extra

main :: IO ()
main = do
    ver <- systemOutput_ "cabal --version"
    let v3 = " 3." `isInfixOf` ver
    retry 3 $ system_ $ "cabal install haskell-src-exts syb unordered-containers" ++ if v3 then " --lib" else ""
    system_ "runhaskell Main test"
    system_ "ghc --make -O2 Main.hs -o uniplate"
    system_ "./uniplate benchmark"
