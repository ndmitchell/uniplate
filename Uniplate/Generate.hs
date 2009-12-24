
module Uniplate.Generate where

import System.Cmd
import System.Directory
import Data.List


generate = do
    gen "Typeable" "Direct"
    gen "Direct" "Typeable"


gen file bad = do
    src <- readFile "Uniplate/Type.hs"
    writeFile "Uniplate/Type.tmp" $ unlines $ filter (not . isInfixOf bad) $ lines src

    writeFile ("Uniplate/" ++ file ++ ".hs") $ unlines
        ["{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses #-}"
        ,"module Uniplate." ++ file ++ " where"
        ,"import Data.Generics.Uniplate." ++ file
        ,"#include \"CommonInc.hs\""]

    system $ "derive Uniplate/Type.tmp >> Uniplate/" ++ file ++ ".hs"
    removeFile "Uniplate/Type.tmp"
