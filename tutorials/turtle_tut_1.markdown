---
title: "Turtle: Scripting in Haskell"
author: Jeffrey Young
date: 2018-05-06
page: turtle_tut_1.html
---

### Scripting in Haskell, an Overview
Scripting in Haskell is made possible through `stack` and the `turtle` library.
Turtle doesn't expect you to know much haskell but the more you know the faster
you will catch on. This tutorial is going to cover some of the basics: the
shebang, comparisons to bash, subroutings and types. So let's get started.

### Some setup
A haskell script is just a haskell file with a special shebang. For convienience
you'll want to run the following in **your global stack**, that is, not in a
haskell stack project.

```hs
stack install turtle       -- The actual turtle library
stack install text         -- A better string library for haskell
```

Now we can start writing our scripts. For this tutorial I'm going to be
programming in `Tut.hs`. So create whatever file you'd like to follow along and
add the following:

```hs
#!/usr/bin/env stack
-- stack --resolver lts-11.7 script
```

If you are on nixOs you'll want to add `--nix` to the stack comment like so:

```hs
-- stack --resolver lts-11.7 script --nix
```

### Running a simple script

Here is the simple hello world example you've been waiting for, complete with
import statements:

```
#!/usr/bin/env stack
-- stack --resolver lts-11.7 script


-- ghc extension to make dealing with strings easier
{-# LANGUAGE OverloadedStrings #-}


import Turtle
import qualified Data.Text as T

main :: IO ()
main = echo "Hey it worked!"
```

As you can see we added the `OverloadedStrings` extension to make our lives
easier. Furthermore we imported turtle and the `text` library so that we can
move from `String`, to `Text`, easily and without worry. The turtle library
rightly uses a lot of `Text` instead of `String` types so while importing `Text`
is not required it does make our lives easier.

and now to run this we can just call it like any other script:

```
osufpc_website/tutorial_code | ./Tut.hs
Hey it worked!

```

So we can run haskell `.hs` files as scripts via stack and with the special
shebang. But because this is haskell we can also compile them to a binary and
run them with all the compiler optimizations applied (they will be much faster).
Here is how to generate such a binary:

```
osufpc_website/tutorial_code | stack ghc -- -O2 -threaded Tut.hs
Linking Tut ...
osufpc_website/tutorial_code | ./Tut
Hey it worked!
```

If you are on Windows and are not going to run this in a `git bash` shell then you'll need to perform the following with admin privileges in a command prompt:

```
assoc .hs=Haskell
ftype Haskell="C:\path\to\stack.exe" "%1" %*
```

refer to the official
[tutorial](https://hackage.haskell.org/package/turtle-1.5.7/docs/Turtle-Tutorial.html)
if you need to.

### More Functions
Alright. We know how to run scripts now but what else does turtle give us? Well the answer is quite a lot. You'll find most of the familiar linux script tools in `Turtle.Prelude` over on its [hackage](https://hackage.haskell.org/package/turtle-1.5.7/docs/Turtle-Tutorial.html)  page.

Here are just a few of the functions that turtle provides out of the box:

```hs
echo  :: MonadIO io => Line -> io ()
cd    :: MonadIO io => Turtle.FilePath -> io ()
pwd   :: MonadIO io => io Turtle.FilePath
touch :: MonadIO io => Turtle.FilePath -> io ()
rm    :: MonadIO io => Turtle.FilePath -> io ()
```

If these type signatures are foreign to you then don't worry about it too much.
As a quick example you can read the signature for `echo` from left to right as
"If I have a type called "io" that forms a MonadIO then I can take a single line
of text (that is a chunk of text with no newlines) and perform some IO action
that returns nothing". Because we are still writing haskell code, we can
leverage `ghci` for type inference like so:

```hs
:t echo  -- :t is a synonym for :type in ghci
echo :: MonadIO io => Line -> io ()
```

But what if there is some system function that we want to call that isn't built
in into turtle? Well we just call the `shell` function. Let's look at the type
signature:

```hs
shell :: MonadIO io => Text -> Shell Line -> io ExitCode
```

So shell takes a `Text` and `Shell Line` which is a type that represents
standard input as lines of `Text` that do not include any newlines, and performs
some `io` action and returns an `ExitCode` from that `io` action. Here is
`shell` in action:

```hs
Research/osufpc_website | stack ghci
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: site
Using main module: 1. Package `site' component exe:site with main-is file: /home/doyougnu/Research/osufpc_website/site.hs
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( /home/doyougnu/Research/osufpc_website/site.hs, interpreted )
Ok, modules loaded: Main.
Loaded GHCi configuration from /tmp/ghci27409/ghci-script
*Main> :m +Turtle
*Main Turtle> :set -XOverloadedStrings
*Main Turtle> shell "true" empty
ExitSuccess
*Main Turtle> shell "ls" empty
about.markdown	_cache        css     deploy.hs  index.markdown  js        _site       site.hs     templates      tutorials
announcements	contact.markdown  deploy  images     in_progress     package.yaml  site.cabal  stack.yaml  tutorial_code  tutorials.html
ExitSuccess
*Main Turtle> shell "pwd" empty
/home/doyougnu/Research/osufpc_website
ExitSuccess
*Main Turtle>
```

As you can see I basically invoked `shell` with the function I want as a string
argument and then I pass `empty` as the second argument because I do not want to
pass any stdin data to the `shell` function. As a quick side note, one can also
use the `sh` function to perform arbitrary shell computations, then only
difference is that `sh` does not return an `ExitCode` e.g. `sh :: MonadIO io =>
Shell a -> io ()`.

### Streams in Turtle
Now we have almost everything we need to make some real scripts! Turtle, like
bash, fsh, zsh, comes equipped with streams. These are represented by the
`Shell` type so we can read the type signature of `ls :: Turtle.FilePath ->
Shell Turtle.FilePath`, it takes a file path, and returns a **stream of file
paths** which makes sense. To use a stream we must consume it using functions of
type `Shell a -> something`. The simplest way to do this is to use the `view`
function. Let's look at the type: `view :: (Show a, MonadIO io) => Shell a -> io
()`. A little more complicated, but not very. This should be read given any type
can be converted to a string (Show a), and anything that forms a `MonadIO`
(MonadIO io), if I am given a Stream of `a`'s then I'll perform an `IO` action
and return nothing from that action.

Okay, but how do we build up streams? Well because of the type of `view` we can
use the monadic `return` function to **lift** a value to the **stream level**.
Or put in code:

```
*Main Turtle> view (return 5)
5
```

So you can think of return having the type `return :: a -> Shell a` for now (it
doesn't but for simplicity just assume this). We can also take anything that
produces an IO action (a subroutine) and transform it into a `Shell` type via a
function called `liftIO :: IO a -> Shell a`.

We can also concatenate two streams via the `(<|>)` operator. So:

```
*Main Turtle> view (return 5 <|> return 10)
5
10
```

Constructs a stream of 5, and a stream of 10, then concatenates them together
and consumes the stream with `view` producing values 5 and 10. We can also use
the `select` function to emit a list of values. This is synonymous with loops in
non-functional languages. This example taken directly from the
[turtle](https://hackage.haskell.org/package/turtle-1.5.7/docs/Turtle-Tutorial.html)
tutorial:

```hs
#!/usr/bin/env stack
-- stack --resolver lts-10.2 script

                                    -- #!/bin/bash
{-# LANGUAGE OverloadedStrings #-}  --
                                    --
import Turtle                       --
                                    --
example = do                        --
    x <- select [1, 2]              -- for x in 1 2; do
    y <- select [3, 4]              --     for y in 3 4; do
    liftIO (print (x, y))           --         echo x y;
                                    --     done;
main = sh example                   -- done
```

We create our first stream of values using `select [1,2]`, now the only values
in that stream are 1 and 2. We then assign that stream to a variable `x` and
perform the same general procedure with another stream `y` and then we print the
values of both out using `liftIO` and `print`. If you look at the types this all makes sense:

```hs
print :: Show a => a -> IO ()
select :: [a] -> Shell a
liftIO :: IO a -> Shell a
```

and then executing this program, it behaves as expected:

```
osufpc_website/tutorial_code | ./Tut.hs
(1,3)
(1,4)
(2,3)
(2,4)
```

You'll notice the use of `sh` instead of `view` in the last example. The two are
equivalent, the only difference being that `sh` does not print the values it is
given e.g:

```hs
*Main Turtle> sh (return 5)
*Main Turtle>
```

In fact we can loop over any stream similar to the example above by selecting a
value from the stream and letting haskell do the rest. Here another good example
from the turtle tutorial that demonstrates this:

```
view :: Show a => Shell a -> IO ()
view s = sh (do
    x <- s -- `x` ranges over every output of `s`
    liftIO (print x))
```

That is we are defining `view`, which consumes a stream and produces an `IO`
action by handing `sh` a **subroutine** via a `do` statement. If you are
unfamiliar with `do` statements, don't worry, you can think of them enforcing an
order to operations much like imperative languages. That is, Once we are in a do
statement the order of operations is persevered to be linear from top to bottom.
So we pick a single element out of our stream, `x <- s`, and then we print it
`liftIO (print x)` and this will repeat until the stream `s` is consumed. Very
nice and clean code.

Let's look at a few more examples of streams and subroutines just to get a feel
for it. Let's say we are trying to write a bunch of data to the file using turtle. For us to be able to do that we must first construct `Shell Line`'s and then call the `output` function like so:

```hs
main = sh writeToFile0

-- | This will overwrite everything in the file
writeToFile0 = do
  x <- select [1..200]
  y <- select ['a'..'z']
  output "tmp" $$ (case textToLine . format w $$ (x, y) of
  Nothing -> ""
  Just x  -> return x)
```

Here we construct two streams, one of integers, one of characters, and we pick
an element from each stream. Then we format a tuple of those elements `format w
$$ (x,y)` to turn them into `Text`, convert the `Text` to a `Line` via
`textToLine` and case match on the `Maybe` result. If we get a `Nothing`, then
we return the empty string, but if we get a value, then we **lift** that value
to the `Shell` level via `return` and pass it to `output "tmp"` which will write
the value to the "tmp" file. This is nice but if you look at the tmp file it
will only contain the last combination of the two streams i.e. `(200, 'z')`.
This is because we are always overwriting the file and not appending to it.
Luckily because we are in Haskell we can use standard Haskell programming to
achieve what we want like so:

```hs
main = sh writeToFile1

writeToFile1 = do
  x <- select [1..200]
  y <- select ['a'..'z']
  liftIO (appendFile "tmp" $$ show (x,y) ++ "\n")
```

I think that is good enough to get you experimenting, make sure to check out the
official [turtle
tutorial](https://hackage.haskell.org/package/turtle-1.5.7/docs/Turtle-Tutorial.html).
In the next post I will be covering command line arguments, how to fold streams,
and how to run external commands.
