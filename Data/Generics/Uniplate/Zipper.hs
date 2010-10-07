{-|
A zipper is a structure for walking a value and manipulating it in constant time.

This module was inspired by the paper:
/Michael D. Adams. Scrap Your Zippers: A Generic Zipper for Heterogeneous Types, Workshop on Generic Programming 2010/.
-}


module Data.Generics.Uniplate.Zipper(
    -- * Create a zipper and get back the value
    Zipper, zipper, zipperBi, fromZipper,
    -- * Navigate within a zipper
    left, right, up, down,
    -- * Manipulate the zipper hole
    hole, replaceHole
    ) where

import Data.Generics.Uniplate.Operations
import Data.Generics.Str
import Control.Monad
import Data.Maybe


-- | Create a zipper, focused on the top-left value.
zipper :: Uniplate to => to -> Zipper to to
zipper = fromJust . toZipper (\x -> (One x, \(One x) -> x))


-- | Create a zipper with a different focus type from the outer type. Will return
--   @Nothing@ if there are no instances of the focus type within the original value.
zipperBi :: Biplate from to => from -> Maybe (Zipper from to)
zipperBi = toZipper biplate


-- | Zipper structure, whose root type is the first type argument, and whose
--   focus type is the second type argument.
data Zipper from to = Zipper
    {reform :: Str to -> from
    ,zipp :: ZipN to
    }

rezipp f (Zipper a b) = fmap (Zipper a) $ f b

instance (Eq from, Eq to) => Eq (Zipper from to) where
    a == b = fromZipper a == fromZipper b && zipp a == zipp b


toZipper :: Uniplate to => (from -> (Str to, Str to -> from)) -> from -> Maybe (Zipper from to)
toZipper biplate x = fmap (Zipper gen) $ zipN cs
    where (cs,gen) = biplate x


-- | From a zipper take the whole structure, including any modifications.
fromZipper :: Zipper from to -> from
fromZipper x = reform x $ top1 $ topN $ zipp x


-- | Move one step left from the current position.
left :: Zipper from to -> Maybe (Zipper from to)
left = rezipp leftN

-- | Move one step right from the current position.
right :: Zipper from to -> Maybe (Zipper from to)
right = rezipp rightN

-- | Move one step down from the current position.
down :: Uniplate to => Zipper from to -> Maybe (Zipper from to)
down = rezipp downN

-- | Move one step up from the current position.
up :: Zipper from to -> Maybe (Zipper from to)
up = rezipp upN


-- | Retrieve the current focus of the zipper..
hole :: Zipper from to -> to
hole = holeN . zipp


-- | Replace the value currently at the focus of the zipper.
replaceHole :: to -> Zipper from to -> Zipper from to
replaceHole x z = z{zipp=replaceN x (zipp z)}


---------------------------------------------------------------------
-- N LEVEL ZIPPER ON Str

data ZipN x = ZipN [Str x -> Zip1 x] (Zip1 x)

instance Eq x => Eq (ZipN x) where
    x@(ZipN _ xx) == y@(ZipN _ yy) = xx == yy && upN x == upN y

zipN :: Str x -> Maybe (ZipN x)
zipN x = fmap (ZipN []) $ zip1 x

leftN  (ZipN p x) = fmap (ZipN p) $ left1  x
rightN (ZipN p x) = fmap (ZipN p) $ right1 x
holeN (ZipN _ x) = hole1 x
replaceN v (ZipN p x) = ZipN p $ replace1 x v

upN (ZipN [] x) = Nothing
upN (ZipN (p:ps) x) = Just $ ZipN ps $ p $ top1 x

topN (ZipN [] x) = x
topN x = topN $ fromJust $ upN x

downN :: Uniplate x => ZipN x -> Maybe (ZipN x)
downN (ZipN ps x) = fmap (ZipN $ replace1 x . gen : ps) $ zip1 cs
    where (cs,gen) = uniplate $ hole1 x


---------------------------------------------------------------------
-- 1 LEVEL ZIPPER ON Str

data Diff1 a = TwoLeft (Str a) | TwoRight (Str a) deriving Eq

undiff1 r (TwoLeft  l) = Two l r
undiff1 l (TwoRight r) = Two l r

-- Warning: this definition of Eq may look too strong (Str Left/Right is not relevant)
--          but you don't know what the uniplate.gen function will do
data Zip1 a = Zip1 [Diff1 a] a deriving Eq

zip1 :: Str x -> Maybe (Zip1 x)
zip1 = insert1 True []

insert1 :: Bool -> [Diff1 a] -> Str a -> Maybe (Zip1 a)
insert1 leftmost c Zero = Nothing
insert1 leftmost c (One x) = Just $ Zip1 c x
insert1 leftmost c (Two l r) = if leftmost then ll `mplus` rr else rr `mplus` ll
    where ll = insert1 leftmost (TwoRight r:c) l
          rr = insert1 leftmost (TwoLeft  l:c) r

left1, right1 :: Zip1 a -> Maybe (Zip1 a)
left1  = move1 True
right1 = move1 False

move1 :: Bool -> Zip1 a -> Maybe (Zip1 a)
move1 leftward (Zip1 p x) = f p $ One x
    where
        f p x = msum $
            [insert1 False (TwoRight x:ps) l | TwoLeft  l:ps <- [p], leftward] ++
            [insert1 True  (TwoLeft  x:ps) r | TwoRight r:ps <- [p], not leftward] ++
            [f ps (x `undiff1` p) | p:ps <- [p]]

top1 :: Zip1 a -> Str a
top1 (Zip1 p x) = f p (One x)
    where f :: [Diff1 a] -> Str a -> Str a
          f [] x = x
          f (p:ps) x = f ps (x `undiff1` p)

hole1 :: Zip1 a -> a
hole1 (Zip1 _ x) = x

-- this way round so the a can be disguarded quickly
replace1 :: Zip1 a -> a -> Zip1 a
replace1 (Zip1 p _) = Zip1 p
