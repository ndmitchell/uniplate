
module Uniplate.Generate where

import System.Cmd
import System.Directory
import Data.List


generate = do
    gen "Typeable" "UniplateDirect"
    gen "Direct" "UniplateTypeable"


gen file bad = do
    orig <- readFile $ "Uniplate/" ++ file ++ ".hs"
    () <- length orig `seq` return ()
    writeFile ("Uniplate/" ++ file ++ ".hs") $ unlines $
        takeWhile (/= "-- GENERATED") (lines orig) ++
        ["-- GENERATED"]

    src <- readFile "Uniplate/Type.hs"
    writeFile "Uniplate/Type.tmp" $ unlines $ filter (not . isInfixOf bad) $ lines src

    system $ "derive Uniplate/Type.tmp >> Uniplate/" ++ file ++ ".hs"
    removeFile "Uniplate/Type.tmp"
