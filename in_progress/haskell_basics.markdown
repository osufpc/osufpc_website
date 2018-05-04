---
title: "Haskell: The Basics Extremely Fast"
author: Jeffrey Young
page: haskell_basics.html
---

### Context
This tutorial is going to go over the basics of haskell incredibly fast, it is a
quick start guide. I'll focus on providing more examples and less words but will
give explanations where I think necessary. This document is meant to bring a
person completely unfamiliar with haskell to a place where they can read the
most basic code. To get the most out of this tutorial you'll want to have `ghci`
installed, preferably through `stack`. If you do not have this then consult the
[stack
tutorial](http://groups.engr.oregonstate.edu/fpc/tutorials/stackOverview.html).
Lastly, I will not try to answer the `why?` question for most of what follows,
if you find yourself yearning for an answer to that question then consult the
references.

#### The birds eye view
Haskell is a pure, immutable, functional, lazy, compiled, strong and statically
typed language. In broad strokes:

##### Purity
Purity means that one cannot perform side effects whenever one wants to. This
means that you cannot perform effects (think of printing to a console) in
addition to returning values. Having purity is important because it ensures
**Referential Transparency** which simply means that given the same inputs to any
function, you **will always** get the same outputs. Only the efficiency may
change. See [1] for more information.

##### Immutability
Immutability means that there is no pass by reference in Haskell. Rather when
  you do something like:

```
let x = x + 1
```

The `x` that is returned is a completely new `x` and the old `x` is preserved
and then eventually garbage collected . This means that you cannot mutate any
variable or data structure `in memory` or rather when you operate on a variable
or data structure, what you get in return is a **different** variable or data
structure that is the result of your computation.

##### Functional
There isn't a formal definition of "functional programming language" but there
is an agreed upon subset of features and computational paradigm. The computation
paradigm is one where computation is modeled via inputs and outputs to
functions, like mathematical functions, not the functions one finds in many
programming languages. This means that you do not have a global state that is
always sitting in the background and being mutated by the program. Furthermore,
you **can only** have expressions (that is statements that always return values)
and not statements which do not return a value (including variable declarations
like `let counter = 0`). Functional programming also means that functions
themselves are first class values. So they can be inputs or outputs to functions
and then can be stored in data structures.

##### Lazy Evaluation
Lazy evaluation defers computation of values until they are required. For
example in an eager language like `C` or `Python`, the arguments to any function
is evaluated before the function body is evaluated. However in Haskell if an
argument is never used, then that argument is also never evaluated. Thus lazy
evaluation avoids unnecessary computations and allows Haskell programmers to
program with infinite data structures.

##### Compiled, Strong Static language
Haskell is compiled, just like `C` or `C++`. While it does have an interpreter
`ghci`, which is useful for debugging, programs are compiled to binaries and
executed just as one would do in the aforementioned cases. Haskell has strong
and static typing, both are related to safety but for our purposes strong typing
means that you cannot work around the type system to treat a type differently
than how the type forces you to treat it. For example, `C` is weakly typed
because you can get any type out of a pointer by casting it to that type. In a
strongly typed language like, Haskell, you cannot do this, if you have an `Int`
then you cannot treat it like a `String` by casting it to a `String` type.
Static typing means that the type system is checked at compile time rather than
at runtime. So if you accidentally concatenate a string with an integer, then your
program will not compile in Haskell, whereas in python you would have to run the
program to watch it explode in order to catch the error.

#### A quick comment about types
When we talk about types in a functional language we are not talking about
memory or the "size" of a type in memory as one would do in languages like `C`.
Rather what we mean is "these things can be treated as type x", or in other
words a type in a functional language such as Haskell can roughly correspond to
a mathematical Set. So the type `Int` is the set of all objects that can be
treated as integers. For example the function to add to integers will have a
type signature that looks like so: `(+) :: Int -> Int -> Int`. Which is read,
starting from the left, as the function takes an `Int`, and another `Int`, and
produces an `Int`. In Haskell, types signatures are not required to be specified
like this, but it is often useful to do so. In Haskell, if you cannot write the
type for your function, then chances are you do not understand what your
function is doing sufficiently to write it! With this in mind let's dive into
some syntax.

#### Syntax and simple types

```haskell
-- I am a comment

-- A haskell file suffix is .hs, and the module must be named the same as the File name
-- so this module will be Foo, with file Foo.hs
-- at the top of our file we write the following

module Foo where

-- the, =, in a module assigns a variable to an expression, in this case a value
-- in ghci you would write a let clause: let x = 2
-- Integers are what you expect
+-------- variable name
|     +-- variable type
|     |
v     v
x :: Int
x = 2

-- we can also have doubles
y :: Double
y = 2.0

-- negative numbers also are what you expect.
z :: Int
z = -2

-- In a function however be careful with the "-" sign this will throw a type error
badExample = 1 + -2    -- this will blow up because "-" is a function!
goodExample = 1 + (-2) -- this is what you want instead

-- Can also have Booleans
myBool :: Bool
myBool = True

-- Characters have single quotes
myChar :: Char
myChar = 'a'

-- Strings are lists of characters, but have built in syntax using double quotes
myString :: String             -- String == [Char]
myStr = "Hello I am a string"

-- Lets start talking about functions, here is a simply unary function that adds 2 to its argument
 +--------------------- Function Name
 |      +-------------- First argument type
 |      |      +------- Result Type
 |      |      |
 v      v      v
foo :: Int -> Int
foo x = x + 2
    ^   ^
    |   |
    |   +------------- Using the bound variable x in the body of the function
    +----------------- Binding the first argument to the variable x

-- Lists have built in syntax and are homogenous
myList :: [Int]
myList = [1, 2, 3, 4, 5]

badList :: [Int]
badList = [1, 'a', 4] -- Blows up because it isn't a list of ints
```


#### References
1. [HaskellWiki: Functional Programming](https://wiki.haskell.org/Functional_programming#Purity)
