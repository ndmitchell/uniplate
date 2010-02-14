
module Uniplate.Fusion(fusion) where

import Control.Exception
import Data.Char
import Data.Generics.Uniplate.Data
import Data.List
import Language.Haskell.Exts.Annotated
import Uniplate.Timer


fusion :: IO ()
fusion = do
    putStrLn "Testing transformBi fusion"
    let file = "Data/Generics/Uniplate/Internal/Data.hs"
    -- let file = "Data.hs"

    putStr "Parsing file... "
    src <- readFile file
    let res = fromParseResult $ parseFileContents $ unlines $ filter (not . isPrefixOf "#") $ lines src
    t <- timer $ evaluate res
    putStrLn $ dp2 t ++ "s"

    putStr "Forcing result... "
    let force = evaluate . length . show
    t <- timer $ force res
    putStrLn $ dp2 t ++ "s"

--    putStr "Prime the cache... "
--    t <- timer $ force (testOld res) >> force (testNew res)
--    putStrLn $ dp2 t ++ "s"

    let old = testOld res
        new = testNew res

    putStr "Transforming new... "
    t <- timer $ force new
    putStrLn $ dp2 t ++ "s"
    
    putStr "Transforming old... "
    t <- timer $ force old
    putStrLn $ dp2 t ++ "s"

    putStrLn $ if old == new then "Identical answers" else "ERROR: Different answers"


type S = SrcSpanInfo
type Id x = x -> x


idName (Ident an x) = Ident an $ map toUpper x
idName x = x

idExp (Var an x) | all isUpper $ prettyPrint x = Lit an $ String an "str" "str"
idExp x = x

idPat (PVar an (Ident _ x)) = PVar an $ Ident an $ reverse x
idPat x = x


testOld :: Module S -> Module S
testOld = transformBi (id :: Id (Exp S))
        . transformBi (idPat :: Id (Pat S))
        . transformBi (id :: Id (Binds S))
        . transformBi (id :: Id (Match S))
        . transformBi (idExp :: Id (Exp S))
        . transformBi (id :: Id (QName S))
        . transformBi (id :: Id (QName S))
        . transformBi (id :: Id (Type S))
        . transformBi (id :: Id (Type S))
        . transformBi (idName :: Id (Name S))
        . transformBi (id :: Id (Name S))

testNew :: Module S -> Module S
testNew = transformBis $ map return
    [transformer (id :: Id (Exp S))
    ,transformer (idPat :: Id (Pat S))
    ,transformer (id :: Id (Binds S))
    ,transformer (id :: Id (Match S))
    ,transformer (idExp :: Id (Exp S))
    ,transformer (id :: Id (QName S))
    ,transformer (id :: Id (QName S))
    ,transformer (id :: Id (Type S))
    ,transformer (id :: Id (Type S))
    ,transformer (idName :: Id (Name S))
    ,transformer (id :: Id (Name S))]
