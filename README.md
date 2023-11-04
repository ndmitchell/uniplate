# Boilerplate Removal with Uniplate [![Hackage version](https://img.shields.io/hackage/v/uniplate.svg?label=Hackage)](https://hackage.haskell.org/package/uniplate) [![Stackage version](https://www.stackage.org/package/uniplate/badge/nightly?label=Stackage)](https://www.stackage.org/package/uniplate)[![Build status](https://img.shields.io/github/actions/workflow/status/ndmitchell/uniplate/ci.yml?branch=master)](https://github.com/ndmitchell/uniplate/actions)

#### Reason for creation

With data structures code that does queries and transformations follows a repetitive code structure of the data type (boilerplate) that can bury the usefulness - application of particular functions. The library focuses on freeing the code and the programmer of that boilerplate and coupling.

#### What it does

Uniplate generics library is simple, concise, and fast, and focused on one task:

1. Simplifies code by decoupling functions from data structure specifics.
2. Does not require special treatment to recursive fields.
3. Simplest generics library, accessible to all Haskell programmers.
4. Is concise.
5. Is most often faster than most generics libraries.

The details on Uniplate were [published at the Haskell Workshop 2007](https://ndmitchell.com/#uniplate_30_sep_2007) (the video presentation, and the thesis chapter).

## Installation

The Uniplate library can be installed with the standard sequence of cabal commands:

    cabal update
    cabal install uniplate

## Using Uniplate

Using library on a simple arithmetic type:

```haskell
{-# LANGUAGE DeriveDataTypeable #-}
module Expr where
import Data.Data
import Data.Generics.Uniplate.Data

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mul Expr Expr
          | Neg Expr
          deriving (Show, Eq, Data, Typeable)
```

Bootstrapping consists of:
* `{-# LANGUAGE DeriveDataTypeable #-}`, [enables automatic deriving](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_extra.html#deriving-data-instances) of `Data` and `Typeable` type classes.
* `Data, Typeable` - automatically derive type classes.
* `import Data.Generics.Uniplate.Data` - imports Uniplate, which leverages 

This enables the simple ["Scrap Your Boilerplate (SYB)"](http://doi.acm.org/10.1145/604174.604179)-based implementation, transparently compatible with other implementations. But simple implementation is 2-8 times slower to compile while using long supported modest extensions of the GHC compiler. Starting with SYB is recommended for its simplicity. Further faster implementation is presented, which requires a bit of type class instance writing.

The Uniplate library defines two classes, `Uniplate` and `Biplate`, along with a number of functions. After importing `Data.Generics.Uniplate.Data` all types which have `Data` instances automatically have the necessary Uniplate instances. In the following subsections we introduce the Uniplate functions, along with examples of using them. The two most commonly used functions are `universe` (used for queries) and `transform` (used for transformations).

### Finding the constant values

```haskell
universe :: Uniplate on => on -> [on]
```

When manipulating our little language it may be useful to know which constants have been used. This can be done with the following code:

```haskell
constants :: Expr -> [Int]
constants x = nub [y | Val y <- universe x]
```

Here the only Uniplate method being used is `universe`, which when given a tree returns the root of the tree, and all its subtrees at all levels. This can be used to quickly flatten a tree structure into a list, for quick analysis via list comprehensions, as is done above.

_Exercise:_ Write a function to test if an expression performs a division by the literal zero.

### Basic optimisation

```haskell
transform :: Uniplate on => (on -> on) -> on -> on
```

If we are negating a literal value, this computation can be performed in advance, so let's write a function to do this.

```haskell
optimise :: Expr -> Expr
optimise = transform f
    where f (Neg (Val i)) = Val (negate i)
            f x = x
```

Here the Uniplate method being used is `transform`, which applies the given function to all the children of an expression, before applying it to the parent. This function can be thought of as bottom-up traversal of the data structure. The optimise code merely pattern matches on the negation of a literal, and replaces it with the literal.

Now let's add another optimisation into the same pass, just before the `f x = x` line insert:

```haskell
f (Add x y) | x == y = Mul x (Val 2)
```

This takes an addition where two terms are equal and changes it into a multiplication, causing the nested expression to be executed only once.

_Exercise:_ Extend the optimisation so that adding `x` to `Mul x (Val 2)` produces a multiplication by 3.

### Depth of an expression

```haskell
para :: Uniplate on => (on -> [res] -> res) -> on -> res
```

Now let's imagine that programmers in your language are paid by the depth of expression they produce, so let's write a function that computes the maximum depth of an expression.

```haskell
depth :: Expr -> Int
depth = para (\_ cs -> 1 + maximum (0:cs))
```

This function performs a paramorphism (a bit like a fold) over the data structure. The function simply says that for each iteration, add one to the previous depth.

_Exercise:_ Write a function that counts the maximum depth of addition only.

### Renumbering literals

```haskell
transformM :: (Monad m, Uniplate on) => (on -> m on) -> on -> m on
```

The literal values need to be replaced with a sequence of numbers, each unique. This is unlikely for an arithmetic expression, but consider bound variables in lambda calculus and it starts to become a bit more plausible:

```haskell
uniqueLits :: Expr -> Expr
uniqueLits x = evalState (transformM f x) [0..]
    where
        f (Val i) = do
            y:ys <- get
            put ys
            return (Val y)
        f x = return x
```

Here a monadic computation is required, the program needs to keep track of what the next item in the list to use is, and replace the current item. By using the state monad, this can be done easily.

_Exercise:_ Allow each literal to occur only once, when a second occurrence is detected, replace that literal with zero.

### Generating mutants

```haskell
contexts :: Uniplate on => on -> [(on, on -> on)]
```

The person who is inputting the expression thinks they made a mistake, they suspect they got one of the values wrong by plus or minus one. Generate all the expressions they might have written.

```haskell
mutate :: Expr -> [Expr]
mutate x = concat [[gen $ Val $ i-1, gen $ Val $ i+1]
                    | (Val i, gen) <- contexts x]
```

The `transform` function is useful for doing an operation to all nodes in a tree, but sometimes you only want to apply a transformation once. This is less common, but is sometimes required. The idea is that the context provides the information required to recreate the original expression, but with this node altered.

_Exercise:_ Replace one multiplication with addition, if there are no multiplications return the original expression.

### Fixed point optimisation

```haskell
rewrite :: Uniplate on => (on -> Maybe on) -> on -> on
```

When slotting many transformations together, often one optimisation will enable another. For example, the the optimisation to reduce.

### Descend

Do something different in the odd and even cases. Particularly useful if you have free variables and are passing state downwards.

### Monadic Variants

```haskell
descendM :: Monad m => (on -> m on) -> on -> m on                         -- descend
transformM :: (Monad m, Uniplate on) => (on -> m on) -> on -> m on        -- transform
rewriteM :: (Monad m, Uniplate on) => (on -> m (Maybe on)) -> on -> m on  -- rewrite
```

All the transformations have both monadic and non-monadic versions.

### Single Depth Varaints

```haskell
children :: Uniplate on => on -> [on]           -- universe
descend :: (on -> on) -> on -> on               -- transform
holes :: Uniplate on => on -> [(on, on -> on)]  -- contexts
```

Lots of functions which operate over the entire tree also operate over just one level. Usually you want to use the multiple level version, but when needing more explicit control the others are handy.

### Evaluation

If we need to evaluate an expression in our language, the answer is simple, don't use Uniplate! The reasons are that there is little boilerplate, you have to handle every case separately. For example in our language we can write:

```haskell
eval :: Expr -> Int
eval (Val i) = i
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Div a b) = eval a `div` eval b
eval (Mul a b) = eval a * eval b
eval (Neg a) = negate a
```

## Using Biplate

All the operations defined in Uniplate have a corresponding Biplate instance. Typically the operations are just the same as Uniplate, with `Bi` on the end.

```haskell
universeBi :: Biplate on with => on -> [with]
transformBi :: Biplate on with => (with -> with) -> on -> on
transformBiM :: (Monad m, Biplate on with) => (with -> m with) -> on -> m on
```

The biggest difference is for the functions `childrenBi` and `descendBi`. In these cases, if the starting type and the target type are the same, then the input value will be returned. For example:

```haskell
childrenBi (Add (Val 1) (Val 2)) == [Add (Val 1) (Val 2)]
children (Add (Val 1) (Val 2)) == [Val 1, Val 2]
```

For example, you should never have `descendBi` in an inner recursive loop.

## Making Uniplate Faster

To make Uniplate faster import `Data.Generics.Uniplate.Direct` and write your instances by hand.

## Acknowledgements

Thanks to Bj&ouml;rn Bringert for feedback on an earlier version of this document, Eric Mertens for various ideas and code snippets, and to Matt Naylor and Tom Shackell for helpful discussions.


## Related work

* [Geniplate](http://hackage.haskell.org/package/geniplate), by Lennart Augustsson, Uniplate compatible but implemented using Template Haskell.
* [Refactoring Uniplate](http://www-ps.informatik.uni-kiel.de/~sebf/projects/traversal.html), by Sebastian Fischer - proposing a slightly different Uniplate API, but with the same underlying concepts.
* [Uniplate for Curry](http://www.informatik.uni-kiel.de/~pakcs/lib/CDOC/Traversal.html), by Sebastian Fischer - using his revised API as above.
* [Uniplate for ML (in MLton)](https://github.com/MLton/mltonlib/blob/master/com/ssh/generic/unstable/public/value/uniplate.sig), as part of the MLton generics library.
* [Uniplate for data types with embedded monads](http://tomschrijvers.blogspot.com/2007/11/extension-proposal-for-uniplate.html), by Tom Schrijvers
* [Multiplate](http://hackage.haskell.org/package/multiplate), by Russell O'Connor, similar ideas to Uniplate but with a very different underlying substrate.
* [Infer.Toys](https://infers.github.io/Infers/Infers.Toys.html) provides a Uniplate-inspired `Elems` module.
