{-# LANGUAGE FlexibleContexts #-}

module Uniplate.HSE(hse) where

import Uniplate.Timer
import Control.Exception
import Data.Generics.Uniplate.Data
import Language.Haskell.Exts.Annotated


hse :: IO ()
hse = do
    putStrLn "Testing speed building dictionaries HSE"
    let sl = SrcLoc "" 0 0
        ssi = toSrcInfo sl [] sl :: SrcSpanInfo

    testN "Module" $ Module ssi Nothing [] [] []
    testN "Decl" $ FunBind ssi []
    testN "Exp" $ Do ssi []
    testN "Pat" $ PWildCard ssi

    putStrLn "Testing speed running over HSE"
    dat <- fmap fromParseResult $ parseFile "Foo.hs"
    evaluate $ length $ show dat
    testN "Universe" dat
    t <- timer $ evaluate $ sum (universeBi $ transformBi (\x -> (x::Int)+1) dat :: [Int])
    putStrLn $ "HSE for Universe/Transform takes " ++ dp2 t ++ "s"


testN :: Biplate a String => String -> a -> IO ()
testN msg x = do
    t <- timer $ evaluate $ length (universeBi x :: [String])
    putStrLn $ "HSE for " ++ msg ++ " takes " ++ dp2 t ++ "s"
