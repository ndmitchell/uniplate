{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Uniplate.DataOnly where
import Data.Generics.Uniplate.DataOnly
import Data.Generics.Uniplate.Data.Instances
#define SKIP_ZIPPER
#include "CommonInc.hs"
