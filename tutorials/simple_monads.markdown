---
title: "Basic Monads: How to use them "
author: Jeffrey Young
date: 2018-08-07
page: simple_monads.html
---

### Introduction
Many people find moving from a beginner to an Intermediate Haskeller is hard
because it means mastering the dreaded monads. But in actuality this is more of
a documentation problem than a theory problem, because when beginners search on
the internet for examples they often find explanations of _what_ the monads are
and _how_ they work. In fact beginner code will almost always use monads without
even knowing it! The purpose of this article is to go over _how_ to use the more
basic monads, not _what_ they are or _how_ they work. If you want to understand
the theory or how they work then there are
[many](http://dev.stephendiehl.com/hask/#monads)
[other](http://learnyouahaskell.com/a-fistful-of-monads)
[resources](https://www.schoolofhaskell.com/school/advanced-haskell/functors-applicative-functors-and-monads)
on the internet for you to read.


### The Maybe Monad
For each monad in this post I'll be going over how to think of them, what their
do notation means, and why we want to use them. A Haskeller at any level of
proficiency will run up against the `Maybe` data type, the difference is that
beginners will run away from the type by pattern matching on it instead of using
the monad instance like this:

```hs
-- | Lets say we have a simple programming language that has
-- variable reference, function definition and application

-- | Variable names.
type Var = String

-- | Abstract syntax.
data Exp = Lit Int          -- integer literal
         | Add Exp Exp      -- addition expression
         | Let Var Exp Exp  -- variable binding
         | Ref Var          -- variable reference
         | Fun Var Exp      -- anonymous function w/ one argument
         | App Exp Exp      -- function application
  deriving (Eq,Show)

-- | Values.
data DVal = DI Int      -- integers
          | DF Var Exp  -- functions
  deriving (Eq,Show)
```

Now we to execute a program in our language we need to write a `sem` function
that evaluates the program and spits out a `DVal`, which are values in our
_semantic domain_. To do so we'll need some way of keeping track of variable
bindings, and we need some way of representing an error. For errors we'll use a
`Maybe` and for state we'll just use a simple `Map` from `Data.Map`.

```hs
type Env a = M.Map Var a

-- | The sem function is going to evaluate a program in our language. It takes a
-- program, and a state which will map variables to values, and may return a
-- value or a Nothing if a Type error occurs here is the type signature
sem :: Exp -> Env DVal -> Maybe DVal
```

Now we can define sem using case statements and pattern matching. This will
probably look very familiar if you've ever done this in an introductory
programming languages course.


```hs
sem :: Exp -> Env DVal -> Maybe DVal
sem (Lit i) _ = Just $ DI i
sem (Add l r) st = case (sem l st, sem r st) of
                      (Just (DI i), Just (DI j)) -> Just . DI $ i + j
                      _                          -> Nothing
sem (Let x e in') st = case sem e st of
                       Just v -> sem in' (M.insert x v st)
                       _ -> Nothing
sem (Ref x)     m = M.lookup x m
sem (Fun x e)   _ = Just (DF x e)
sem (App l r)   m = case (sem l m, sem r m) of
                       (Just (DF x e), Just v) -> sem e (M.insert x v m)
                       _ -> Nothing
```

When we see a literal we simple wrap it in our `Maybe DVal` type, when we want
to perform addition we recursively evaluate the left hand side, and the right
hand side. If we get values back then we perform the addition and wrap the
result value in our `Maybe DVal` data type. If something goes wrong then we
return a `Nothing` to represent the error. The `Let` construct is interesting;
we evaluate the expression our variable `x` will be bound to, which is `e` in
this example. If we get a value, `v`, back then we manipulate our store to map
our variable `x` to our value `v` and then we recursively evaluate the `in'`
expression, where the `x` variable will presumably be called. It may not look
like it, but this pattern of carrying around a state in a function parameter,
and then performing this recursion with an updated state is actually a state
monad hiding in plain sight. If we see a variable reference `Ref` then we simply
lookup its value form the store and return it. For the function definition we
simple mirror the function in our semantic domain and finally application will
proceed almost identically to addition, the only difference is that we
deconstruct through pattern matching the function definition and recur into the
function body.

That code will work just fine, but it is not exactly elegant and we aren't
utilizing the maybe monad as effectively as we could be. Instead we would be
better served to use _do notation_ to take advantage of the `Maybe` monad
directly. Here is what the `sem` function would look like using do notation:

```hs
-- | Semantic function.
semMaybe :: Exp -> Env DVal -> Maybe DVal
semMaybe (Lit i)    _ = return $ DI i
semMaybe (Add l r) st = do (DI l') <- semMaybe l st
                            (DI r') <- semMaybe r st
                            return . DI $ l' + r'
semMaybe (Let x e in') st = do v <- semMaybe e st
                                semMaybe in' (M.insert x v st)
semMaybe (Ref x)     m = M.lookup x m
semMaybe (Fun x e)   _ = return (DF x e)
semMaybe (App l r)   m = do (DF x e) <- semMaybe l m
                             v <- semMaybe r m
                             semMaybe e (M.insert x v m)

```

Whenever we talk about a monad we need to think about the _computational
context_ that it is encapsulating. For the `Maybe` monad this context
encapsulates _failure_. What this means is that when we are in _do notation_ for
a `Maybe` if any line in the do notation returns a Nothing then the entire
computation will return a `Nothing` value. Which is quite nice! For example with
the `Add` case above, if our left hand side or our right hand side _do not_
return a value but return something else, perhaps something we have not pattern
matched on, then because we are in do notation, for a `Maybe` the entire result
will become a `Nothing`. Another important detail of do notation is that every
line must return a type `m a`, which means it must return a monad type. Thus,
for this example, you'll notice that we never have to write a `Just` constructor
to pattern match on the result of a recursive call. This is because `Just` is a
value of our monad, namely the `Maybe` monad. So in this example the type of `m
a` in our do notation unifies to `Maybe DVal` where `m` is the `Maybe` monad,
and our value, `a` is `DVal`.

### The State Monad
Our `semMaybe` is a direct improvement over the first `sem` function. Not only
is the code cleaner but it also describes the desired computation more
succinctly than the previously. However, we still are carrying around a peculiar
input to our function, namely our variable store. It would be much better if we
didn't have to explicitly pass around our store and instead could just reference
it when needed. This is the computational context that the State monad
encapsulates. Let's step away from our little language and consider some more
simple examples. First lets just have a stateful loop that counts to ten and
returns something; those that are imperatively inclined should find this example
enlightening:

```hs
import Control.Monad.State -- import our state monad from the mtl library

-- A state monad is of type State s a, where s is the type of the state you want
-- to keep, and a is the type of the return value
-- type MyState a = State Int a

-- Now we may want to increment our state, just like i++; as if one were writing
-- in a C-like language
increment :: MyState ()
increment = do st <- get         -- we retrieve the old state and name it st
               put (succ st)     -- we call succ on it, and then replace
                                 -- the old state with the new one
```

The `increment` function is calling two convenient functions from the `mtl`
library, namely `get`, and `put`. Let's take a peek at their type signatures:

```hs
get :: m s

put :: m s -> ()
```

`get` simple returns the state and `put` takes a new state, and then replaces
the old state with the new state and returns a unit value.

Now that we can increment our state, we need to write our loop and return
something once our state reaches, say, 10.

```hs
stateLoop :: a -> MyState a
stateLoop x = do st <- get
                 increment
                 if st >= 10
                 then return x
                 else stateLoop x
```
