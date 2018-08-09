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
-- Our imports for this file
import Control.Monad.State
import qualified Data.Map as M


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
sem (Lit i) _ = Just $$ DI i
sem (Add l r) st = case (sem l st, sem r st) of
                      (Just (DI i), Just (DI j)) -> Just . DI $$ i + j
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
semMaybe (Lit i)    _ = return $$ DI i
semMaybe (Add l r) st = do (DI l') <- semMaybe l st
                            (DI r') <- semMaybe r st
                            return . DI $$ l' + r'
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
get :: MonadState s m => m s

put :: MonadState s m => s -> m ()
```

`get` simple returns the state, `m s` and `put` takes a new state `s`, and then
replaces the old state with the new state and returns a unit value. We can also
use a convenient function called `modify` to avoid these `get` and `put` calls
like so:

```hs
increment2 :: MyState ()
increment2 = modify succ
```

As always let's look at the type signature:

```hs
> :t modify
modify :: MonadState s m => (s -> s) -> m ()
```

So now we can see how this works. Modify is a higher-ordered function that takes
some function `(s -> s)` that takes a state and does something with it. In our
case this is simply adding one to our state.

Do notation for a state monad is threading the state for every line. What this means is that we do not need to explicitly return arguments, put the state, get the state, and apply functions. Instead, we can just operate on the state implicitly through the do notation. Here is what I mean in code:

```hs
-- | incBy3 will take any state and increment it 3 times
incBy3 :: MyState ()
incBy3 = do increment
            increment
            increment
```

You'll notice that the `incBy3` function is never getting the state or putting
the state. Rather this is all hidden in the increment function. What is
beautiful about this is that we have a very declarative langauge for describing
what our code does. We do not need to deal with any of the under-the-hood
details of getting the state and incrementing it manually we can simply call
`increment` and rely on do-notation to ensure that the state the second
`increment` sees, is a **result** of the first `increment` call. Just like
do-notation in a `Maybe` monad encapsulates failure, and hence every line in the
do-notation could fail or succeed, do-notation for a `State` monad encapsulates
stateful computation, where every line could perform some computation on the
state, or with the state.

Now that we can increment our state, let's write a loop and return
something once our state reaches, say, 10.

```hs
stateLoop :: a -> MyState a
stateLoop x = do st <- get
                 increment
                 if st >= 10
                 then return x
                 else stateLoop x
```

In order to run anything in the state monad we must use one of three functions:
`runState`, `evalState`. Here are their type signatures.

```hs
runState :: State s a -> s -> (a, s)

evalState :: State s a -> s -> a

execState :: State s a -> s -> s
```

So you can see that `runState` takes a state monadic value `State s a`, and an
initial state `s`, and returns a tuple that holds the final value of the
computations `a`, and the final state `s`. `execState` and `evalState`, are
similar funcitons but instead of returning a tuple they return either the last
state `s`, or the final value `a`.

Let's see if our function actually works now:

```hs
            |-- This will create a (State Int String), s = Int, a = String
            |              |- This is our initial state
            V              V
> runState (stateLoop "a") 0
("a",11)

> evalState (stateLoop "a") 0
"a"

> execState (stateLoop "a") 0
11
```

But what happens if we send in an initial state that is 10 or more?

```hs
> runState (stateLoop "a") 100
("a",101)
```

Ah looks like we are incrementing the state before we perform the check which is
extra computation we don't necessarily want. Let's implement a fix:

```hs
stateLoop2 :: a -> MyState a
stateLoop2 x = do st <- get
                  if st >= 10
                    then return x
                    else do increment2
                            stateLoop2 x
```

Much better. You'll notice that I've nested do notation here. This is fine as
long as the monads for each do does not change. If it does then you'll need some
function to convert the inner do monad, to the other (this is often a `lift`
function). Alright let's test it out:

```hs
> runState (stateLoop2 "a") 100
("a",100)


> runState (stateLoop2 "a") 0
("a",10)
```

Great! Looks like it works as we've intended. Hopefully you will have seen that
there really is nothing to be scared of with the State monad, it is a valuable
and powerful tool in our toolbox and we should try to use it when it makes sense
to.

### Combining the Maybe and State Monads
We've seen the result of rewriting our semantic function using a `Maybe` monad,
and we know that we have a `State` monad hiding in plain sight, so one might
want to combine these two to gain the benefits of both. Such a person may try
something like this:

```hs
type BadEnv a = State (Env DVal) (Maybe a)

sem'' :: Exp -> BadEnv DVal
sem'' (Lit i) = return . Just $$ DI i
sem'' (Add l r) = do (Just (DI l'')) <- sem'' l
                      (Just (DI r'')) <- sem'' r
                      return . Just . DI $$ l'' + r''
sem'' (Let x b e) = do res <- sem'' b
                       modify $$ M.insert x res
                       sem'' e
...
```

Even though it seems reasonable that you'd set your return value for `State` to
a `Maybe a`, you won't get far with this approach. Not only are you having to
pattern match on the `Maybe` constructors (which we wanted to avoid in the first
place) but when you go try to call modify haskell will get confused because
modify is expecting a `DVal` but you are now passing it a `Maybe DVal` so you'll
have to deconstruct the `Maybe` type manually, which is exactly what we were
trying to avoid by using the `Maybe` monad in the first place!

The appropriate way to do this is to use **Monad Transformer Stacks**. I won't
go into a ton of detail here on them but I'll show how to construct one for this
case. Instead of our `BadEnv` type we'll use a `StateT` **monad transformer** to
embed our `Maybe` _in_ our `State` monad, like so:

```hs
              |- The StateT Monad Transformer
              |        |- The state we want to carry around
              |        |          |- The inner monad
              |        |          |   |- The return type
              V        V          V   V
type Env' a = StateT (Env DVal) Maybe a
```

The `StateT` monad transformer takes a state `s`, a inner monad `m` and a return
type `a` and returns a monad transformer stack that embeds the inner monad into
a state monad. These type variables are apparent in the `runStateT`,
`evalStateT` and `execStateT` functions:

```hs
> :t runStateT
runStateT :: StateT s m a -> s -> m (a, s)

> :t execStateT
execStateT :: Monad m => StateT s m a -> s -> m s

> :t evalStateT
evalStateT :: Monad m => StateT s m a -> s -> m a
```

So now instead of having a `State s a` we have a `StateT s m a`, which means
that when we run the `StateT` Monad Transformer we'll get our inner monad back
as output, which in this case is a `Maybe` monad.

Alright, let's write our `sem` function using our monad transformer stack:

```hs
sem' :: Exp -> Env' DVal
sem' (Lit i) = return $$ DI i
sem' (Add l r) = do DI l' <- sem' l
                    DI r' <- sem' r
                    return . DI $$ l' + r'
sem' (Let x b e) = do res <- sem' b
                      modify $$ M.insert x res
                      sem' e
sem' (Ref x) = do st <- get
                   lift $$ M.lookup x st
sem' (Fun x e)   = return $$ DF x e
sem' (App l r)   = do st <- get
                      DF x e <- sem' l
                      v <- sem' r
                      modify $$ M.insert x v
                      sem' e
```

Ah quite nice! Let's step through it case by case. For literals we just return
our literal after wrapping in a `DVal`. It is important to note that this
`return` is _lifting_ a non-monadic DVal _into_ the monad transformer stack. So
that return doesn't just lift the value to a `Maybe`, or to a `State` monad, it
lifts it to a `StateT s Maybe` monad stack. The Add case is identical to the
`Maybe` monad case. The let case recursively evaluates the `b` expression that
we will be binding `x` to, as long as that succeeds then we add our binding for
`x` to the result of `b` in our state with a call to `modify`, and finally recur
to the "in" portion of the let binding, which in this case is the `e` variable.
To grab a reference from our store we simply get the state, look it up, and
return it. You'll notice the use of a function called `lift` here. This is often
used with monad transformer stacks because it allows one to take a value from
the inner monad and **lift** it to the outer monad. So in this case our call to
`M.lookup` returns a `Maybe` value, because our do-notation is for our outer
monad, and therefore every line must have type `StateT`, haskell will throw a
type error because it cannot match a `Maybe` with a `StateT`. To lift our
`Maybe` to a `StateT` we simply will call `lift` on it. Here is the type
signature for `lift`:

```hs
> :t lift
lift :: (Monad m, MonadTrans t) => m a -> t m a
```

Where `m` is the inner monad and `t` is the outer monad transformer. In our case
`m = Maybe` and `t = StateT` so the type of lift in the `sem` function above
will unify to:

```hs
lift :: Maybe a -> StateT Maybe a
```

Moving on we have function definition which is identical to the `Maybe` monad
case and finally function application. For function application we get our
state, check that the left hand side evaluates to a function, and our right hand
side evaluates to a value. If both do then we add the binding of `x` to `v` to
our state and recur into the function body `e`. Now our `sem'` function takes
only a program in our langauge and possbily returns a value if everything goes
well; a much clearer type signature than our previous `sem` function had. Let's
just make sure that we can actually evaluate a program in our language:

```hs
-- | We define a successor function which takes a 'x' and adds a literal 1 to it.
-- we then apply this function to a five, and again to the result of that application
exSucc :: Exp
exSucc = Let "succ" (Fun "x" (Add (Ref "x") (Lit 1)))
             (App (Ref "succ") (App (Ref "succ") (Lit 5)))

   |- we want to run our state transformer monad
   |          |- dsem' constructs a type of StateT (Env DVal) Maybe a
   |          |            |- an empty state is an empty map
   v          V            V
> runStateT (dsem' exSucc) M.empty
Just (DI 7,fromList [("succ",DF "x" (Add (Ref "x") (Lit 1))),("x",DI 6)])

> evalStateT (dsem' exSucc) M.empty
Just (DI 7)

> execStateT (dsem' exSucc) M.empty
Just (fromList [("succ",DF "x" (Add (Ref "x") (Lit 1))),("x",DI 6)])
```

As you can see we got back a `Maybe (Dval, Env DVal)`. As you construct deeper
monad transformer stacks you'll find yourself having to unwrap each successive
monad from the top down. So here our top monad was a `StateT` and our bottom
monad was a `Maybe`, so we needed to unwrap the `StateT` with `runStateT` and
then we can access the `Maybe` monad. For monads which do not have a `Show`
instance, such as the `Reader` monad, you would need to call `runReader` on the
**result** of the `runStateT`, or `execStateT`, or `evalStateT` call, depending
on what you want to do.

But what happens if we have a type error in our program:


```hs
-- | Here we are defining succ to be a literal, and then trying to use that in
-- functional application as if it were a function
exSucc2 :: Exp
exSucc2 = Let "succ" (Lit 2)
             (App (Ref "succ") (App (Ref "succ") (Lit 5)))
```

```hs
> runStateT (dsem' exSucc2) M.empty
Nothing

> execStateT (dsem' exSucc2) M.empty
Nothing

> runStateT (dsem' exSucc2) M.empty
Nothing
```

Just as expected we receive a `Nothing` back because of our type error. So not
only have do we have more expressive types (types that describe the computation
of a function), but we have more declarative, easier to understand code, and
