# Boilerplate Removal with Uniplate [![Hackage version](https://img.shields.io/hackage/v/uniplate.svg?label=Hackage)](https://hackage.haskell.org/package/uniplate) [![Stackage version](https://www.stackage.org/package/uniplate/badge/nightly?label=Stackage)](https://www.stackage.org/package/uniplate) [![Build status](https://img.shields.io/travis/ndmitchell/uniplate/master.svg?label=Build)](https://travis-ci.org/ndmitchell/uniplate)

Generic transformations and queries are often referred to as boilerplate code - they remain relatively similar as the action performed by the code changes, and can often outnumber the actual intent of the code in terms of lines. While other generic traversal schemes have shown how powerful new features can be added to compilers, and how the type system can be manipulated into accepting these operations, the Uniplate library focuses on a conceptually simpler generic concept. A more complete document on Uniplate was published at the Haskell Workshop 2007, and is available from [here](https://ndmitchell.com/#uniplate_30_sep_2007), along with a video presentation, and the associated thesis chapter.

Uniplate is a simple, concise and fast generics library. To expand on that sentence:

1. A generics library is one which allows you to write functions that operate over a data structure without tying down all aspects of the data structure. In particular, when writing an operation, you don't need to give a case for each constructor, and you don't have to state which fields are recursive.
2. Uniplate is the simplest generics library. Using Uniplate is within the reach of all Haskell programmers.
3. Uniplate is more concise than any other generics library.
4. Uniplate is fast, not always the absolute fastest, but massively faster than many generics libraries.
5. Uniplate is also less powerful than some other generics libraries, but if it does the job, you should use it.

The Uniplate library can be installed with the standard sequence of cabal commands:

    cabal update
    cabal install uniplate

This document proceeds as follows:

1. Using Uniplate
2. Using Biplate
3. Making Uniplate Faster

#### Acknowledgements

Thanks to Bj&ouml;rn Bringert for feedback on an earlier version of this document, Eric Mertens for various ideas and code snippets, and to Matt Naylor and Tom Shackell for helpful discussions.

## Using Uniplate

To demonstrate the facilities of Uniplate, we use a simple arithmetic type:

<pre>
<b>{-# LANGUAGE DeriveDataTypeable #-}</b>
module Expr where
<b>import Data.Data</b>
<b>import Data.Generics.Uniplate.Data</b>

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mul Expr Expr
          | Neg Expr
          deriving (Show, Eq, <b>Data, Typeable</b>)
</pre>

In this definition, the Uniplate specific bits are bolded. The three extra parts are:

* `import Data.Generics.Uniplate.Data`, this module contains all the Uniplate functions and definitions.
* `deriving (Data,Typeable)`, this deriving clause automatically adds the necessary instances for Uniplate.
* `{-# LANGUAGE DeriveDataTypeable #-}`, this pragma turns on language support for the deriving line.

This definition makes use of the [Scrap Your Boilerplate (SYB)](http://doi.acm.org/10.1145/604174.604179) based Uniplate implementation. The SYB implementation is compatible with the other implementations, but is slower (between 2 and 8 times) and requires some modest compiler extensions (implemented in [GHC](http://haskell.org/ghc/) for many years). The alternative definition scheme is described towards the end of this document, in "Making Uniplate Faster". I recommend using the SYB implementation to start with, as it requires least work to use.

The Uniplate library defines two classes, `Uniplate` and `Biplate`, along with a number of functions. After importing `Data.Generics.Uniplate.Data` all types which have `Data`instances automatically have the necessary Uniplate instances. In the following subsections we introduce the Uniplate functions, along with examples of using them. The two most commonly used functions are `universe` (used for queries) and `transform` (used for transformations).

### Finding the constant values

    universe :: Uniplate on => on -> [on]

When manipulating our little language it may be useful to know which constants have been used. This can be done with the following code:

    constants :: Expr -> [Int]
    constants x = nub [y | Val y <- universe x]

Here the only Uniplate method being used is `universe`, which when given a tree returns the root of the tree, and all its subtrees at all levels. This can be used to quickly flatten a tree structure into a list, for quick analysis via list comprehensions, as is done above.

_Exercise:_ Write a function to test if an expression performs a division by the literal zero.

### Basic optimisation

    transform :: Uniplate on => (on -> on) -> on -> on

If we are negating a literal value, this computation can be performed in advance, so let's write a function to do this.

    optimise :: Expr -> Expr
    optimise = transform f
        where f (Neg (Val i)) = Val (negate i)
              f x = x

Here the Uniplate method being used is `transform`, which applies the given function to all the children of an expression, before applying it to the parent. This function can be thought of as bottom-up traversal of the data structure. The optimise code merely pattern matches on the negation of a literal, and replaces it with the literal.

Now let's add another optimisation into the same pass, just before the `f x = x` line insert:

    f (Add x y) | x == y = Mul x (Val 2)

This takes an addition where two terms are equal and changes it into a multiplication, causing the nested expression to be executed only once.

_Exercise:_ Extend the optimisation so that adding `x` to `Mul x (Val 2)` produces a multiplication by 3.

### Depth of an expression

    para :: Uniplate on => (on -> [res] -> res) -> on -> res

Now let's imagine that programmers in your language are paid by the depth of expression they produce, so let's write a function that computes the maximum depth of an expression.

    depth :: Expr -> Int
    depth = para (\_ cs -> 1 + maximum (0:cs))

This function performs a paramorphism (a bit like a fold) over the data structure. The function simply says that for each iteration, add one to the previous depth.

_Exercise:_ Write a function that counts the maximum depth of addition only.

### Renumbering literals

    transformM :: (Monad m, Uniplate on) => (on -> m on) -> on -> m on

The literal values need to be replaced with a sequence of numbers, each unique. This is unlikely for an arithmetic expression, but consider bound variables in lambda calculus and it starts to become a bit more plausible:

    uniqueLits :: Expr -> Expr
    uniqueLits x = evalState (transformM f x) [0..]
        where
            f (Val i) = do
                y:ys <- get
                put ys
                return (Val y)
            f x = return x

Here a monadic computation is required, the program needs to keep track of what the next item in the list to use is, and replace the current item. By using the state monad, this can be done easily.

_Exercise:_ Allow each literal to occur only once, when a second occurrence is detected, replace that literal with zero.

### Generating mutants

    contexts :: Uniplate on => on -> [(on, on -> on)]

The person who is inputting the expression thinks they made a mistake, they suspect they got one of the values wrong by plus or minus one. Generate all the expressions they might have written.

    mutate :: Expr -> [Expr]
    mutate x = concat [[gen $ Val $ i-1, gen $ Val $ i+1]
                      | (Val i, gen) <- contexts x]

The `transform` function is useful for doing an operation to all nodes in a tree, but sometimes you only want to apply a transformation once. This is less common, but is sometimes required. The idea is that the context provides the information required to recreate the original expression, but with this node altered.

_Exercise:_ Replace one multiplication with addition, if there are no multiplications return the original expression.

### Fixed point optimisation

    rewrite :: Uniplate on => (on -> Maybe on) -> on -> on

When slotting many transformations together, often one optimisation will enable another. For example, the the optimisation to reduce.

### Descend

Do something different in the odd and even cases. Particularly useful if you have free variables and are passing state downwards.

### Monadic Variants

    descendM :: Monad m => (on -> m on) -> on -> m on                         -- descend
    transformM :: (Monad m, Uniplate on) => (on -> m on) -> on -> m on        -- transform
    rewriteM :: (Monad m, Uniplate on) => (on -> m (Maybe on)) -> on -> m on  -- rewrite

All the transformations have both monadic and non-monadic versions.

### Single Depth Varaints

    children :: Uniplate on => on -> [on]           -- universe
    descend :: (on -> on) -> on -> on               -- transform
    holes :: Uniplate on => on -> [(on, on -> on)]  -- contexts

Lots of functions which operate over the entire tree also operate over just one level. Usually you want to use the multiple level version, but when needing more explicit control the others are handy.

### Evaluation

If we need to evaluate an expression in our language, the answer is simple, don't use Uniplate! The reasons are that there is little boilerplate, you have to handle every case separately. For example in our language we can write:

    eval :: Expr -> Int
    eval (Val i) = i
    eval (Add a b) = eval a + eval b
    eval (Sub a b) = eval a - eval b
    eval (Div a b) = eval a `div` eval b
    eval (Mul a b) = eval a * eval b
    eval (Neg a) = negate a


## Using Biplate

All the operations defined in Uniplate have a corresponding Biplate instance. Typically the operations are just the same as Uniplate, with `Bi` on the end.

    universeBi :: Biplate on with => on -> [with]
    transformBi :: Biplate on with => (with -> with) -> on -> on
    transformBiM :: (Monad m, Biplate on with) => (with -> m with) -> on -> m on

The biggest difference is for the functions `childrenBi` and `descendBi`. In these cases, if the starting type and the target type are the same, then the input value will be returned. For example:

    childrenBi (Add (Val 1) (Val 2)) == [Add (Val 1) (Val 2)]
    children (Add (Val 1) (Val 2)) == [Val 1, Val 2]

For example, you should never have `descendBi` in an inner recursive loop.

## Making Uniplate Faster

To make Uniplate faster import `Data.Generics.Uniplate.Direct`, and provide Uniplate instances by generating them with the [Derive tool](http://community.haskell.org/~ndm/derive/).

## Related work

<ul>
	<li><a href="http://hackage.haskell.org/package/geniplate">Geniplate</a>, by Lennart Augustsson, Uniplate compatible but implemented using Template Haskell.</li>
    <li><a href="http://www-ps.informatik.uni-kiel.de/~sebf/projects/traversal.html">Refactoring Uniplate</a>, by Sebastian Fischer - proposing a slightly different Uniplate API, but with the same underlying concepts.</li>
	<li><a href="http://www.informatik.uni-kiel.de/~pakcs/lib/CDOC/Traversal.html">Uniplate for Curry</a>, by Sebastian Fischer - using his revised API as above.</li>
    <li><a href="http://mlton.org/">Uniplate for ML (in MLton)</a>, it used to be under <tt>mltonlib/trunk/com/ssh/generic/</tt>, but I can't find it anymore.</li>
	<li><a href="http://tomschrijvers.blogspot.com/2007/11/extension-proposal-for-uniplate.html">Uniplate for data types with embedded monads</a>, by Tom Schrijvers</li>
	<li><a href="http://hackage.haskell.org/package/multiplate">Multiplate</a>, by Russell O'Connor, similar ideas to Uniplate but with a very different underlying substrate.</li>
</ul>
