
import System.Process.Extra
import Data.List
import Control.Monad
import Control.Exception.Extra
import System.Info

main :: IO ()
main = when (os == "linux") $ do
    -- The script was only designed for Linux, so restrict it for now
    ver <- systemOutput_ "cabal --version"
    let v3 = " 3." `isInfixOf` ver
    retry 3 $ system_ $ "cabal install haskell-src-exts syb unordered-containers" ++ if v3 then " --lib" else ""
    system_ "runhaskell Main test"
    system_ "ghc --make -O2 Main.hs -o uniplate"
    system_ "./uniplate benchmark"
