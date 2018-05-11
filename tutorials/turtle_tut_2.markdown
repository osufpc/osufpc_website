---
title: "Turtle: Command Line arguments, IO and Folding Streams"
author: Jeffrey Young
date: 2018-05-10
page: turtle_tut_2.html
---

### Last Time
Last time I tried to introduce some of the basics of the turtle library for
scripting with Haskell. I thought that post was too word heavy, in this post I
want to have more code snippets that we can actually analyze and talk about. So
I'll be going over command line arguments, basic IO, folding streams of data and
running external commands.

### Command Line Arguments
Let's look at the simplest possible script. We'll import some command line
arguments and then immediately print them out.

```hs
import Turtle.Options -- required for command line argument handling
import Prelude hiding (FilePath) -- this will conflict with a type in Options

parser :: Parser (Text, Text)
parser = (,) <$$> argText "firstArg" "The First Argument"
             <*> argText "secondArg" "The Second Argument"

argDemo :: IO ()
argDemo = do (first, second) <- options "A basic demo" parser
             echo $$ repr first   -- use repr to convert to a Line
             echo $$ repr second
```

And when you run this little script with no arguments you'll be greeted with an
auto-generated help message:

```
osufpc_website/tutorial_code | ./Tut.hs
A basic demo

Usage: Tut.hs FIRSTARG SECONDARG

Available options:
  -h,--help                Show this help text
  FIRSTARG                 The First Argument
  SECONDARG                The Second Argument

```

And if we run it with only one argument we'll receive:

```
osufpc_website/tutorial_code | ./Tut.hs one
Missing: SECONDARG

A basic demo

Usage: Tut.hs FIRSTARG SECONDARG

Available options:
  -h,--help                Show this help text
  FIRSTARG                 The First Argument
  SECONDARG                The Second Argument

```

And if we run with both then it'll work as we expect:

```
osufpc_website/tutorial_code | ./Tut.hs one two
"one"
"two"
```

Now you may be a little concerned about the crazy parser function. In fact you
might have never seen applicative functors like that before. I won't go over
them in detail, but suffice it to say that they are somewhere between a functor,
and a monad, see chapter 11 in [LYAH](http://learnyouahaskell.com/chapters) if
you want to get into it. The way that they are being used here is one of the beautiful things about haskell. In essence we are describing an applicative parser. The particular form in we are using it requires some more detailed explantion. First let's look at types:

```hs
Prelude> :t (,)
(,) :: a -> b -> (a, b)

Prelude> :t (<$$>)
(<$$>) :: Functor f => (a -> b) -> f a -> f b

Prelude> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
```

From the top, the `(,)` tuple operator is also a function! The function takes two arguments and constructs a tuple, this is true at the type level as well. The `(,)` **type constructor**  takes two types and constructs the tuple type. In fact let's look at the **type constructor's kind signature**:

```hs
Prelude> :k (,)
(,) :: * -> * -> *
```

The `:k` is shorthand for `:kind` in `ghci`. Alright now what about the weird dollar sign operator? Well it may look familiar to you if you've been around haskell for a bit. Here let me jog your memory:

```hs
Prelude> :t map
map :: (a -> b) -> [a] -> [b]

Prelude> :k ([])
([]) :: * -> *

Prelude> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

Everyone is familiar with the `map` function. It takes a function as an
argument, and a list, and applies the function to every element of the list,
resulting in a list of `[b]`'s. But look at the kind signature of the list type
constructor. It takes a type and produces a type, namely the type of a list of
elements of the type it was given. So if you don't give it a type, then it will
be expecting a type argument to get to kind `*`. Don't believe me just check the
kind for a non-empty list:

```
Prelude> :k ([] Int)
([] Int) :: *
```

So we can now explain the real type of `map`; it is actually a **less general**
instance of `fmap`. Fmap's type signature reads as follows: Given anything that
is a `Functor`, named `f`, and a function from `a` to `b`, and a `Functor` of
a's, I'll return a functor of b's. This all works because the `Functor` type has
kind `* -> * -> Constraint`. Let's just ask ghci to make sure:

```hs
Prelude> :k Functor
Functor :: (* -> *) -> Constraint
```

So when we supply a type to our functor `f`, which must have kind `* -> *`, say
we get a `Constraint` back, which the function uses to make sure it cannot be
called on anything that does not implement `Functor`. So the `f` will be bound
to anything that has a unary kind, just like the list type constructor does.
Hence fmap is equivalent to map when `f` is `[]`. Here is a derivation if you
are having trouble:

```
fmap :: Functor f => (a -> b) -> f a -> f b   -- sub f for []
fmap :: Functor f => (a -> b) -> [] a -> [] b -- now wrap for syntactic sugar
fmap :: Functor f => (a -> b) -> [a] -> [b]   -- now remove Constraint because we know lists implement Functor
fmap :: (a -> b) -> [a] -> [b]

Prelude> :t map
map :: (a -> b) -> [a] -> [b]
```

If this is the first time you are seeing all this then congratulations, you are
on your way to intermediate haskell! Alright now on to the funky function call. Remember we had this for our parser:

```
parser :: Parser (Text, Text)
parser = (,) <$$> argText "firstArg" "The First Argument"
             <*> argText "secondArg" "The Second Argument"
```

Well now we can kinda figure it out a bit. We are fmapping `<$$>`, the tuple
function `(,)` over some other stuff with another combinator `<*>`. I won't go
into applicatives in more detail but suffice it to say that the reason we need a
`<*>` is because we had two arguments to our function. In fact if we had three
arguments then we would need two `<*>`'s to separate the three arguments. Like
this:

```hs
-- One argument
(+1) <$$> pure 2

-- Two arguments
(+) <$$> pure 1 <*> pure 2

-- Three arguments
(\x y z -> x + y + z) <$$> pure 1 <*> pure 2 <*> pure 10
```

And so on. You'll notice I have to call `pure` before all these arguments. That
is because I need to **lift** them to the applicative level before I can run the
computation. Applicative are awesome, in fact you can use them on anything that implements the `Applicative` type class:

```hs
-- Like lists
Prelude> (*) <$$> [1..10] <*> [5..15]
[5,6,7,8,9,10,11,12,13,14,15,10,12,14,16,18,20,22,24,26,28,30,15,18,21,24,27,30,33,36,39,42,45,20,24,28,32,36,40,44,48,52,56,60,25,30,35,40,45,50,55,60,65,70,75,30,36,42,48,54,60,66,72,78,84,90,35,42,49,56,63,70,77,84,91,98,105,40,48,56,64,72,80,88,96,104,112,120,45,54,63,72,81,90,99,108,117,126,135,50,60,70,80,90,100,110,120,130,140,150]

-- Or Maybes
Prelude> (*) <$$> Just 5 <*> Just 10
Just 50

Prelude> (*) <$$> Just 5 <*> Nothing
Nothing

-- and even functions themselves
Prelude> (*) <$$> (+5) <*> (+2) $$ 5
70

Prelude> (+) <$$> (+5) <*> (+2) $$ 5
17
```

Ok so back to the parser and our program
```hs
parser :: Parser (Text, Text)
parser = (,) <$$> argText "firstArg" "The First Argument"
             <*> argText "secondArg" "The Second Argument"

argDemo :: IO ()
argDemo = do (first, second) <- options "A basic demo" parser
             echo $$ repr first
             echo $$ repr second
```

We are taking the result of the first call to argText (before the `<*>`) and
putting it in the first position of the tuple because of that is where the `a`
in the tuple function type signature goes: (,) :: a -> b -> (a, b), and then we
do the same thing with the second argument, which gives us a `Parser (Text,
Text)`. `Text` because that is what is returned from `argText`, and Parser
because that matches on the `f` in `fmap`, hence it has kind `Parser :: * -> *`.

We then call the `options` which takes a description and a parser (to parse the
command line options) and it'll return the data we wanted in a tuple for us to
use. But what happens if instead of text we send in a number?

```
osufpc_website/tutorial_code | ./Tut.hs 1 2
"1"
"2"
```

Ah darn, anything that implements `Show` can be parsed to a `Text`. We should instead use `argInt` to tell turtle that we are expecting an integer:

```hs
intParser :: Parser (Int, Int)
intParser = (,) <$$> argInt "firstInt" "The First Integer"
                <*> argInt "secondInt" "The Second Integer"

argDemo :: IO ()
argDemo = do (first, second) <- options "A basic demo" intParser -- changed parser
             echo $$ repr first
             echo $$ repr second
```

```
osufpc_website/tutorial_code | ./Tut.hs 1 2
1
2
```

Awesome, and we only needed to change our parser...and if we input a string now?

```
osufpc_website/tutorial_code | ./Tut.hs 1 "Imastring"
A basic demo

Usage: Tut.hs FIRSTINT SECONDINT

Available options:
  -h,--help                Show this help text
  FIRSTINT                 The First Integer
  SECONDINT                The Second Integer

```

Ah, we get back the usage text. The last thing for command line arguments I'll
go over will be optional arguments. Let's assume that we must have the first
argument, but only need the second argument sometimes. Well luckily turtle
handles this through the `optional` function, which is applied to our parser
like so:

```hs
optIntParser :: Parser (Int, Maybe Int)
optIntParser = (,) <$$> argInt "firstInt" "The First Integer"
                   <*> optional (argInt "optional secondInt" "The Second Integer now not required")


argDemo :: IO ()
argDemo = do (first, second) <- options "A basic demo" intParser
             echo $$ repr first
             echo $$ repr second
```

and now because we are returning a `Maybe`, we can not pass the second
parameter, but we will still be carrying around the `Maybe` type.

```hs
osufpc_website/tutorial_code | ./Tut.hs 1
1
Nothing

osufpc_website/tutorial_code | ./Tut.hs 1 2
1
Just 2

osufpc_website/tutorial_code | ./Tut.hs 1 "a string"
A basic demo

Usage: Tut.hs FIRSTINT [OPTIONAL SECONDINT]

Available options:
  -h,--help                Show this help text
  FIRSTINT                 The First Integer
  OPTIONAL SECONDINT       The Second Integer now not required

```

### Basic IO
This one will be a short section. Basically turtle handles IO through `stdin`
and `stdout` functions. They do exactly what you expect i.e. `stdin` reads in a
`Line` one line at a time, and stdout reads out a `Line` one line at a time. The
turtle tutorial shows this by defining cat:

```
cat' = stdout stdin

main = cat'
```

and if we run that it'll reciprocate anything we input until with `Ctrl-C` out
of it. There is also the useful `input` and `output` utilities for writing out
to files.

```hs
main :: IO ()
main = dumpTofile "tmp"

dumpTofile file = output file . return . mconcat $ ((repr .) .) . (,,) <$> [1.200] <*> ['a'..'z'] <*> ['a'..'z']
```

You'll notice I did some fancy intermediate tricks here. Starting from the
right, I use applicatives to construct a list of 3-tuples that will have every
combination of `[1..200]`, `['a'..'z']`, and `['a'..'z']`. I then convert it to
`[Line]` by composing a ternary function `(,,)` with a unary function `repr`.
Then because lists are monoids I can collapse them to a single value using
`mconcat :: Monoid a => [a] -> a`. I then lift that result to the `Shell` level
via `return` and send that to the output function which will write to the file. If we run the script we'll get what we expect, namely a file full of junk data:

```
```
