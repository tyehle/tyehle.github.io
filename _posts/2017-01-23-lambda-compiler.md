---
layout: post
title: Compiling and Interpreting the Lambda Calculus in Haskell
published: true
---

This whole project is based on [this post by Matt Might](http://matt.might.net/articles/compiling-up-to-lambda-calculus). I decided I would try to implement it in Haskell instead of Racket. There are several steps of the process that are missed because they are built into the Racket language. In my implementation I needed a parser, an interpreter, and an extractor, in addition to a reimplementation of the compiler.

Instead of defining all the functions in the language in the compiler, I thought it would be more fun to write them in the language itself. This greatly reduces the complexity of the parser and the compiler. Also it means you can add any library functions you see fit, and I ended up adding some comparisons, and list operations.

All code for the project can be found [on github](https://github.com/tyehle/lambda).

### Sections
{: .no_toc}

- Dummy list
{:toc}


------


Parser
------
Because many functions are implemented in the language itself, the grammar quite small.

```
<program> ::= <define> ... <exp>

<module>  ::= <define> ...

<define>  ::= (define <var> <exp>)
           |  (define (<var> <arg> ...) <exp>)

<exp>     ::= <var>

           |  <nat>

           |  (<lam> (<arg> ...) <exp>)
           |  (let ((<var> <exp>) ...) <exp>)
           |  (letrec (<var> <exp>) <exp>)

           |  (<exp> <exp> ...)

<arg>     ::= _ | <var>

<lam>     ::= λ | lambda
```

`<arg>` is its own form because I like using `_` as an ignored argument.

I added `program`, `module` and `define` forms to make it possible to do bootstrapping.


I used [parsec](https://hackage.haskell.org/package/parsec) to get the grammar into this data structure.

```haskell
data Program = Program [Definition] Exp deriving (Show)

data Definition = Def String Exp deriving (Show)

data Exp = Var String

         | Num Int

         | Lambda [String] Exp
         | Let [(String, Exp)] Exp
         | Letrec String Exp Exp

         | Application Exp Exp
         deriving (Show)
```

I also wrote a very basic "lexer" to remove comments.


This isn't a parsec tutorial, so I won't bore you with details of the parser. The interface to the parser is

```haskell
parseProgram :: String -> String -> Either String Program
parseProgram = lexParse programP

parseModule :: String -> String -> Either String [Definition]
parseModule = lexParse moduleP

lexParse :: Parser a -> String -> String -> Either String a
lexParse p name input = first show $ parse commentP name input >>= parse p name
```

I had never used [bifunctors]() before, but it sure made handling `Either` easier.

This will result in a `Left` with the shown version of the `ParseError` if the parser fails. Throughout the project I used `Left String` to represent error.

Every stage of the compiler has the type `a -> Either String b` so it can all be strung together with bind.


--------


Base Definitions
----------------

The bootstrapped definitions of basic functions in the language are in [resources/base.lc](https://github.com/tyehle/lambda/blob/master/resources/base.lc).

All the desugarings implemented in [Matt Might's post](http://matt.might.net/articles/compiling-up-to-lambda-calculus) that are missing from the compiler are here.

There are also some new definitions that make actually writing programs a bit easier.

### Useful Functions

```racket
(define (id x) x)
(define (const x _) x)
(define hang
  ((λ (u) (u u)) (λ (u) (u u))))
```

`const` will work the same as in Haskell because the interpreter is lazy. `(const x hang)` won't evaluate the second argument, so it won't hang.

`hang` is used to kill the program if `head` or `tail` is called on an empty list. Might seem a bit draconian, but I didn't implement errors or exceptions cause they're hard. So too bad.


### Booleans

All booleans are [church encoded](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans). Church booleans are functions that take two arguments; what to do if its true, and what to do if its false.

- true = `(λ (t f) t)`
- false = `(λ (t f) f)`

Since the interpreter is lazy `if` can be safely represented as a function because the branch that is not taken will never be evaluated.

```racket
(define (if c t f)
  (c t f))
```

All other boolean operations can be defined in a similar way, eg. `not`.

```racket
(define (not a)
  (a #f #t))
```


### Church Numerals

All things are done using [Church Numerals](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals).

A church numeral is a function that takes two arguments: an operation to perform `f` and an object to operate on `x`. For the church numeral n, the operation is done n times to `x`.

- 1 = `(λ (f x) (f x))`
- 0 = `(λ (f x) x)`
- 3 = `(λ (f x) (f (f (f x))))`


### Numeric Operations

The new numeric operations are defined as follows.


```racket
(define (even? n) (n not #t))
```

`even?` is its own function because a general remainder operator using repeated subtraction is (I think) less efficient.
When the the number is evaluated it will apply `not` to `#t` n times.

You may have noticed this will take at least linear time as opposed to constant time one would expect from a real programming language. This is because church numerals are slow and should never be used unless you like [office chair battles](https://xkcd.com/303/).


```racket
(define (/ n m)
  (letrec (div1 (λ (n1 m1)
                  (let ([diff (- n1 m1)])
                    (if (zero? diff)
                        0
                        (succ (div1 diff m1))))))
    (div1 (succ n) m)))
```

The division algorithm is repeated subtraction, and is described [on wikipedia](https://en.wikipedia.org/wiki/Church_encoding#Division).

`div1` is fast (because it only does one subtraction each iteration), but it is not quite the division operator we want.

- `(div1 2 3) = 0` as expected
- `(div1 4 3) = 1` as expected
- `(div1 3 3) = 0`, but should be 1

To fix this problem we just need to add one to the input `n`. This fix would create problems with negative numbers, but there are no negative numbers in this language, so its all fine.


```racket
(define (mod n m)
  (letrec (mod1 (λ (n1 m1)
                  (let ([diff (- n1 m1)])
                    (if (zero? diff)
                        n1
                        (mod1 diff m1)))))
    (prev (mod1 (succ n) m))))
```

`mod` is the same algorithm as division, but instead of counting the number of iterations, we just return `n1`.

The fix is much the same; just add 1 to the input. Since its the final value of the input we are interested in we then subtract one from the result.


```racket
(define (<= a b)
  (zero? (- a b)))
(define (>= a b)
  (zero? (- b a)))
(define (< a b)
  (<= (+ 1 a) b))
(define (> a b)
  (>= a (+ 1 b)))
```

All the new comparison operations check if the result of a subtraction is zero.


### Lists

Church encoded lists are similar to booleans. They are functions that take a thing to do if the list is a pair, and a result if the list is empty. If the list is a pair then the function is given two arguments, the head and the tail. If the list is empty then the result is passed straight through.

```racket
(define (cons h t)
  (λ (f _) (f h t)))
(define empty
  (λ (_ e) e))
```

This encoding will only work with a lazy interpreter. The argument `e` must only be evaluated if the list is empty.

Using these definitions we can define common list operations, some of which I have listed below.

```racket
(define (head l)
  (l (λ (h _) h) hang))
(define (tail l)
  (l (λ (_ t) t) hang))
(define (pair? l)
  (l (λ (_ _) #t) #f))
(define (null? l)
  (l (λ (_ _) #f) #t))
```

As I said earlier, `head` and `tail` will hang if called on the empty list because there isn't anything else we could reasonably do.

`pair?` and `null?` never evaluate the contents of the list, so a program like `(pair? (cons hang hang))` will still terminate.

```racket
(define (from n)
  (cons n (from (succ n))))
```

`from` builds an infinite list starting at the given value. Since the interpreter is lazy this is not a problem.

```racket
(define (take n l)
  (if (or (zero? n) (null? l))
      empty
      (cons (head l) (take (prev n) (tail l)))))
```

`take` results in a list containing the first n elements of the given list. It will return fewer than n items if the given list is not long enough.

```racket
(define (foldl fn acc xs)
  (if (null? xs)
      acc
      (foldl fn (fn acc (head xs)) (tail xs))))

(define (foldr fn acc xs)
  (if (null? xs)
      acc
      (fn (head xs) (foldr fn acc (tail xs)))))
```

Folds work here like they do in every other language.

The lazy interpreter allows the right fold to exit early.

A right fold that gets the first element if it exists, `(foldr (λ (e _) #t) #f (from 0))`, will terminate, but the left fold version will not.

```racket
(define (range low high)
  (if (>= low high)
      empty
      (cons low
            (range (+ 1 low)
                   high))))
```

This definition is just more efficient than `(take (- high low) (from low))` because church numerals suck.

All the other language features must be implemented by the compiler.


--------


Compiler
--------

The purpose of the compiler is to transform the high level AST into one that represents the pure lambda calculus. The target structure is

```haskell
data Node = Lam String Node
          | Ref String
          | App Node Node
          deriving (Show, Eq)
```


### Scope and Free Variables

There are a bunch of places in the compiler and interpreter where knowing which variables will be referenced later is important.

Any variable that has no binding is considered free.

- `(λ (x) (f x))` -- `f` is free and `x` is bound
- `(λ (y) (λ (x) (y x)))` -- `x` and `y` are both bound
- `((λ (x) x) x)` -- `x` is  bound inside the lambda but free outside

The compiler needs to know which variables are free to determine which definitions can be dropped, and the interpreter needs to know which variables are free to handle closures properly.

Both the `Node` and `Exp` structures need a `freeVars` function, so they implement a typeclass.

```haskell
class Scope a where
  freeVars :: a -> Set String
```

This allows a convenient function for guards in the compiler and interpreter.

```haskell
isFree :: Scope e => String -> e -> Bool
isFree name expr = name `Set.member` freeVars expr
```

For the `Node` structure, `Lam` is the only way to bind a variable, and `Ref` is the only way to reference one.

```haskell
instance Scope Node where
  freeVars (Lam arg body) = Set.delete arg $ freeVars body
  freeVars (Ref name) = Set.singleton name
  freeVars (App f x) = freeVars f `Set.union` freeVars x
```

For `Exp`, variables can be bound by `Lambda`, `Let` or `Letrec`, and referenced by `Var`.

```haskell
instance Scope Exp where
  freeVars (Var name) = Set.singleton name
  freeVars (Lambda args body) = freeVars body `removeAll` args
  freeVars (Let bindings body) = foldr addBindingVars bodyVars bindings
    where
      addBindingVars = Set.union . freeVars . snd
      bodyVars = freeVars body `removeAll` map fst bindings
  freeVars (Letrec name binding body) = Set.delete name allVars
    where
      allVars = freeVars binding `Set.union` freeVars body
  freeVars (Num _) = Set.empty
  freeVars (Application f x) = freeVars f `Set.union` freeVars x
```


### Define

The define forms desugar to a series of `let` or `letrec` expressions.

```haskell
desugarDefs :: [Definition] -> Program -> Exp
desugarDefs baseDefs (Program defs expr) = desugarProgram newProg
  where
    newProg = Program (baseDefs ++ defs) expr

desugarProgram :: Program -> Exp
desugarProgram (Program ds e) = foldr defToLet e ds
  where
    defToLet (Def name expr) body = if isFree name body
                                    then if isFree name expr
                                         then Letrec name expr body
                                         else Let [(name, expr)] body
                                    else body
```

`desugarDefs` integrates the definitions of a module into those of a program.

`desugarProgram` only let-binds a definition if it is used, and then only does a `letrec` if the name being defined is free in its definition. Otherwise it desugars to the much simpler `let` form.

After this stage the program is represented as a single expression.


### Easy Prey: Variables, Application and Numbers

References and applications have a one-to-one mapping. Numbers become repeated function application.

```haskell
compileExp (Var name) = Ref name
compileExp (Application a b) = compileExp a `App` compileExp b
compileExp (Num n) = churchNum n

churchNum :: Int -> Node
churchNum n = Lam "f" $ Lam "x" $ foldr App (Ref "x") $ replicate n (Ref "f")
```


### Multi-Argument Lambdas

The lambda form of the high level AST accepts multiple arguments. The lambda calculus only has single argument lambdas, so they need to be curried.

```haskell
compileExp (Lambda args body) = foldr Lam (compileExp body) args
```

This will change an expression like `(λ (x y z) body)` to `(λ (x) (λ (y) (λ (z) body)))`.

Function application is already curried by the parser, so there is nothing to fix up there.

### Let

A `let` is changed into an immediately applied lambda.

- `(let ([x 5]) (+ x 3))` becomes `((λ (x) (+ x 3)) 5)`
- `(let ([x a] [y b]) body)` becomes `((λ (x) ((λ (y) body) b)) a)`

```haskell
compileExp (Let [] body) = compileExp body
compileExp (Let ((n,v):rest) body) = Lam n inner `App` compileExp v
  where
    inner = compileExp (Let rest body)
```

### Letrec

To understand how the Y combinator allows recursion, it is sufficient to know that it is a function with the property `(Y G) = (G (Y G))`.

I'll run through a quick example of defining a function `F` as some an arbitrary expression `exp` that can call `F`.
The only way to bring a variable into scope is using lambda, so we might as well start there.

`(λ (F) exp)`

We can then use `Y` to get a definition for `F`.

`(Y (λ (F) exp))` expands to `((λ (F) exp) (Y (λ (F) exp)))` in the same way that `(Y G)` expands to `(G (Y G))`. We can then go ahead and evaluate `exp` with `F` bound to `(Y (λ (F) exp))`. If ever `F` is called inside `exp`, the Y combinator will expand again to produce another definition of `F` to call "recursively".

The feeling I get about the Y combinator is this. Every time you try to evaluate `(Y G)` it expands to `(G (Y G))`, allowing you to continue a recursive computation in `G` for as long as you wish. From the interpreter's perspective its not really recursion, its just expanding a copy of the same function definition as many times as necessary.

In the terms of the `(λ (F) exp)` example `(letrec (F exp) body)` is equivalent to `(let ([F (Y (λ (F) exp))]) body)`

My first thought when I saw this was: Hang on! The `F` bound in that let and the `F` in the definition aren't the same. That's variable shadowing!

And this is true from the standpoint of the interpreter, but the property of the Y combinator means that the two definitions of `F` are equivalent. Which is why it is a definition of recursion without recursion.

Here is the implementation in the compiler. The definition of the Y combinator still seems a bit like devil magic to me, but I thought [this answer](https://cs.stackexchange.com/a/9651) and [Matt Might's post](http://matt.might.net/articles/implementation-of-recursive-fixed-point-y-combinator-in-javascript-for-memoization/) made a lot of sense.

```haskell
compileExp (Letrec name binding body) = compileExp recursiveLet
  where
    recursiveLet = Let [(name, y `Application` Lambda [name] binding)] body

-- (λy.λF.F (λx.y y F x))(λy.λF.F (λx.y y F x))
y :: Exp
y = term `Application` term
  where
    term = Lambda ["y", "F"] $ Var "F" `Application` Lambda ["x"] innerApp
    innerApp = Var "y" `Application`
               Var "y" `Application`
               Var "F" `Application`
               Var "x"
```


-----------


Interpreter
-----------

The task of the interpreter is to reduce a lambda term. But what does it mean to be reduced? Presumably we would like a result that can be easily interpreted by a human. The most important reduction we can do is a beta reduction, which is the same as a function call. `(λx.body) arg` would reduce to `body` with a new variable in scope `x = arg` (with some caveats about what variables are in scope for `arg`).

Beta-reduction is good enough for some programs. `(= (+ (* 3 3) (* 4 4)) (* 5 5))` gives `λt.λf.t`, which is the church encoding for true.

Boolean results are pretty easy to see, but numbers get out of hand really quickly. The simple program `(+ 2 3)` results in `(λb.(λa.λf.λx.a f (b f x)) (λf.λx.f (f x))) (λf.λx.f (f (f x)))`. The interpreter didn't even do any work here. It has essentially told us that the answer is 2+3.

This is clearly not a solution. Instead, the interpreter should give an answer as a Haskell primitive. With the knowledge that `(* 6 7)` should be a number we can un-church the result to get `42`.

### Call by Need
I said earlier that the interpreter is lazy. The goal is to make programs like `((λ (_) 42) hang)` work. When a function call is evaluated the argument is stored in a thunk. If ever that argument is used, then its value is computed, and the thunk is replaced with the computed value.

This requires state.

Data stored in the environment is either a thunk, or a forced result.

```haskell
data Box s = Forced (Result s) | Thunk Node (Env s)
```

I use `ST` for mutation of the environment. All of those `s` variables are references to the state thread used to make sure state does not escape the `ST` monad. The environment is a map of variable names to a box.

```haskell
type Env s = Map String (STRef s (Box s))
```

### Why Use ST?
All of the other examples of interpreters I could find online used `IORef` instead of `STRef` for mutable boxes to store thunk results.

The reason they gave was that the interpreter was already dealing with `IO`. That is totally reasonable and makes their code much more simple.

Unfortunately my interpreter is not coupled at all to `IO`, so I had to use `ST`. This led to some interesting issues with impredictive types, and I ended up needing the Rank2Types language extension. More on that later.

### Result Type

The result of a program could be a number, boolean, closure, or a list of results. I also have a function result type to help un-churching numbers and lists.

```haskell
data Result s = Clos String Node (Env s)
              | RFun (Result s -> ExceptT String (ST s) (Result s))
              | RNum Integer
              | RBool Bool
              | RPair (Result s) (Result s)
              | REmpty
```

The `RFun` type transforms a result, and can possibly fail. The increment operator needed to decode numbers is defined as an `RFun`.

```haskell
plus1 :: Result s -> ExceptT String (ST s) (Result s)
plus1 (RNum n) = return $ RNum (n+1)
plus1 _ = throwE "Cannot increment non-number type"
```

The type `ExceptT String (ST s) (Result s)` is a bit long and I use it a lot, so I made an alias.

```haskell
type EST s a = ExceptT String (ST s) a
```

### Interpreting An expression

The meat of the interpreter is the function `lazyInterp`

```haskell
lazyInterp :: Env s -> Node -> EST s (Result s)
lazyInterp env (Lam arg body) = return $ Clos arg body env
lazyInterp env (Ref x) = maybe (scopeExcept x) force (Map.lookup x env)
lazyInterp env (App f x) = lazyInterp env f >>= (`app` Thunk x env)

scopeExcept :: String -> EST s a
scopeExcept = ExceptT . return . scopeError
```

There are only three forms, so only three cases to handle.

- Lambdas immediately build a closure
- References force a box from the environment, possibly failing if there is no variable in scope
- Applications force the function, build a thunk for the argument, and then call out to an application function.

The `scopeExcept` function just changes the type of `scopeError` to `EST s a` instead of `Either String a` like is used the the rest of the pipeline.

To force a box, we check if its a thunk, if so, call `lazyInterp` and write the result.

```haskell
force :: STRef s (Box s) -> EST s (Result s)
force ref = lift (readSTRef ref) >>= handleBox
  where
    handleBox (Forced r) = return r
    handleBox (Thunk node env) = do
      result <- lazyInterp env node
      lift $ writeSTRef ref (Forced result)
      return result
```

The type of the `app` function may seem a bit strange. I wanted to use it in the interpreter, and also to un-church results. Having it take a box lets me do this.

```haskell
app :: Result s -> Box s -> EST s (Result s)
app (RFun f) (Forced x) = f x
app (RFun f) (Thunk node env) = lazyInterp env node >>= f
app (Clos arg body env) box = do
  argRef <- lift $ newSTRef box
  let extendedEnv = Map.insert arg argRef env
  lazyInterp extendedEnv body
app _ _ = throwE "Cannot apply non-function type"
```

Unlike closures, applying an `RFun` is strict in its argument. Fortunately `RFun` always needs its argument so this behavior is fine.

### Extracting a Result

Once an expression has been interpreted, it is a closure. I then use an extraction function to end up with a Haskell literal.

The type of all the extractors is `Result s -> EST s a`, where a is the type of the literal. There are extractors for integers, booleans, and lists.

Integers can be extracted by applying the increment function and zero to the church encoded number.

`(n inc 0)`

Operations in our interpreter can fail, so each application needs to use bind. Once we have the final result we can unwrap it from the `Result` data type.

```haskell
intExtractor :: Result s -> EST s Integer
intExtractor res = res `app` Forced (RFun plus1)
               >>= (`app` Forced (RNum 0))
               >>= getNum
  where
    plus1 (RNum n) = return $ RNum (n+1)
    plus1 _ = throwE "Cannot increment non-number type"
    getNum (RNum n) = return n
    getNum _ = throwE "Non-number result"
```

Booleans have a similar story, but here we apply the values True then False `(b True False)`.

```haskell
boolExtractor :: Result s -> EST s Bool
boolExtractor res = res `app` Forced (RBool True)
                >>= (`app` Forced (RBool False))
                >>= getBool
  where
    getBool (RBool b) = return b
    getBool _ = throwE "Non-boolean result"
```

Lists are more complicated. The list extractor first needs an extractor for the elements in the list. This ensures you can extract arbitrarily nested lists.

Lists are encoded as a function that takes something to do with a head and a tail and a result if the list is empty. To extract a literal list from this structure we apply an `onCons` function and the `REmpty` result.

`onCons` is a function that takes two results and yields an `RPair`. It looks a little funny because it has to be curried to fit into the application model of the interpreter.

To extract a list from a pair we run the extractor on the first of the pair to get a literal, and run the list extractor on the tail.

```haskell
listExtractor :: (Result s -> EST s a) -> Result s -> EST s [a]
listExtractor ex res = res `app` Forced (RFun onCons)
                   >>= (`app` Forced REmpty)
                   >>= pairToList
  where
    onCons hd = return . RFun $ return . RPair hd
    pairToList REmpty = return []
    pairToList (RPair hd tl) = do
      x <- ex hd
      xs <- listExtractor ex tl
      return (x:xs)
    pairToList _ = throwE "Non-list result"
```

I lied a little bit. There is one more extractor.

My unit tests for early versions of the interpreter expected the output to be a `Node`, not the `Result` type I ended up with. Instead of rewriting all of the tests to expect a closure, I extract a `Node` from a closure.

The only variables that need to be bound from the closure's environment are the ones that are free in the body. Dropping the environment from the closure, and instead binding those variables with immediately applied lambdas gets the job done.

```haskell
unwrap :: Result s -> EST s Node
unwrap (Clos arg body env) = bindAll toBind $ Lam arg body
  where
    toBind = Set.toList $ arg `Set.delete` freeVars body
    bindAll [] expr = return expr
    bindAll (x:xs) expr = do
      binding <- maybe (scopeExcept x) force $ Map.lookup x env
      rawBinding <- unwrap binding
      bindAll xs $ Lam x expr `App` rawBinding
unwrap _ = throwE "Cannot unwrap non-closure type"
```

These three extractors result in `EST s lit`, not `Either String lit` like the rest of the pipeline. The last step is to run the stateful computation using `runST`.

{% comment %}

This was the most confusing part of the project, so I wrote up a few examples to make things more clear.

#### Extracting 2

The literal 2 is compiled to `λf.λx.f (f x)`. When interpreted this becomes a closure with an empty environment `{λf.λx.f (f x), []}`.

To extract the number, first apply `plus1`

- ``{λf.λx.f (f x), []} `app` plus1``
- `interp [f=plus1] λx.f (f x)`
- `{λx.f (f x), [f=plus1]}`

then apply `0`

- ``{λx.f (f x), [f=plus1]} `app` 0``
- `interp [f=plus1, x=0] f (f x)`
  - `interp [..] f => plus1`
  - `interp [..] f x`
    - `interp [..] f => plus1`
    - `interp [..] x => 0`
  - `1`
- `2`

#### Extracting [True]
{% raw %}

The expression `(cons #t empty)` is evaluated to `{λf.λe.f h t, [h={λt.λf.t,[]}, t={λf.λe.e,[]}]}`.

To extract a list, first apply `onCons`

- ``{λf.λe.f h t, [h={λt.λf.t,[]}, t={λf.λe.e,[]}]} `app` onCons``
- `interp [f=onCons, h={..}, t={..}] λe.f h t`
- `{λe.f h t, [f=onCons, h={..}, t={..}]}`

then apply `REmpty`

- ``{λe.f h t, [f=onCons, h={..}, t={..}]} `app` REmpty``
- `interp [e=REmpty, f=onCons, h={..}, t={..}] f h t`
  - `interp [e,f,t,h] f h`
    - `interp [e,f,t,h] f => onCons`
    - `interp [e,f,t,h] h => {λt.λf.t,[]}`
  - `RFun \tail -> {{λt.λf.t,[]} x tail}`
  - `interp [e,f,t,h] t`
  - `{λf.λe.e,[]}`
- `{{λt.λf.t,[]} x {λf.λe.e,[]}}`

convert the result to a list

- `{{λt.λf.t,[]} x {λf.λe.e,[]}}`
  - `{λt.λf.t,[]} => True`
  - extract a list from `{λf.λe.e,[]}`
    - apply `onCons`
      - ``{λf.λe.e,[]} `app` onCons``
      - `interp [f=onCons] λe.e`
      - `{λe.e,[f=onCons]}`
    - apply `REmpty`
      - ``{λe.e,[f=onCons]} `app` REmpty``
      - `interp [e=REmpty, f=onCons] e => REmpty`
    - convert the result to a list
      - `[]`
- `True : []`

{% endraw %}

{% endcomment %}

### Running a Stateful Computation

The only drawback to `ST` is the type of `runST`.

```haskell
runST :: forall a. (forall s. ST s a) -> a
```

Functions in Haskell normally only have type variables on the outside of a type.

```haskell
func :: forall a b. a -> b
```

They type of `runST` ensures that the resulting type cannot have any reference to the state thread, `s`.

The type `forall s a. ST s a -> a` could unify to `ST s (Result s) -> Result s`. This would be bad because a reference to the state thread would escape the stateful computation. This could cause side effects, and if you want that then Haskell is not the right language for you.

The type `forall a. (forall s. ST s a) -> a` cannot unify to `ST s (Result s) -> Result s` because the type variable `s` is not in scope on the right side of the function. This ensures that no references to the state thread escape.

Types of this form are beyond Haskell's type inference engine. There is a special type rule for `$` that allows code like `runST $ do {...}`. Unfortunately, the function for extracting lists requires `s` in its type signature, and the additional powers of the `$` operator are not enough to make the type checker succeed.

To make this code work, I enabled the `{-# LANGUAGE Rank2Types #-}` extension. This allows all the extraction functions to have types that match the rest of the pipeline.

```haskell
extract :: (forall s. Result s -> EST s a) -> Node -> Either String a
extract ex input = runST $ runExceptT (lazyInterp Map.empty input >>= ex)

extractInt :: Node -> Either String Integer
extractInt = extract intExtractor

extractBool :: Node -> Either String Bool
extractBool = extract boolExtractor

extractList :: (forall s. Result s -> EST s a) -> Node -> Either String [a]
extractList ex = extract $ listExtractor ex

interp :: Node -> Either String Node
interp = extract unwrap
```

To interpret and extract an int from a compiled program you could now call `extractInt prog` and get either an int or an error message.

---------------------


Tying it all Together
---------------------

Each piece of the pipeline transforms the program until it is a Haskell literal. The following example shows how each piece fits together by following a simple program through each stage.

### The Pieces

Start with the input program `(cons #t empty)`.

Parse the program to get:

```
Program [] (Application (Application (Var "cons") (Var "#t")) (Var "empty"))
```

The internal data structures aren't very easy to read, so the rest of this is in nicer notation.

Desugar the program form to a single expression.

```
(let ([#t (λ (t f) t)])
  (let ([cons (λ (h t)
                (λ (f _)
                  ((f h) t)))])
    (let ([empty (λ (_ e) e)])
      (cons #t empty))))
```

Compile the program to the lambda calculus.

```
(λ#t.(λcons.(λempty.cons #t empty)
            (λ_.λe.e))
     (λh.λt.λf.λ_.f h t))
(λt.λf.t)
```

Interpret the program to get a closure.

```raw
{λf.λ_.f h t, [t={λ_.λe.e,[]}, h={λt.λf.t,[]}]}
```

Extract a pair from the closure.

```
< {λt.λf.t,[]} x {λ_.λe.e,[]} >
```

Finally, extract a list from the pair.

```
[True]
```

### The Interface

I wanted to run programs from strings, and from files in the repl. I also wanted to interpret a program and pretty print the resulting lambda. `runProgram`, `runFile` and `prettyProgram` are the functions that do these things. `runDisplayProgram` contains all the common code between them.

```haskell
type Interp a = Node -> Either String a

runDisplayProgram :: (a -> IO ()) -> String -> String -> Interp a -> IO ()
runDisplayProgram display filename input extractor = do
  base <- readBase
  either putStrLn display $ do
    defs <- base
    prog <- parseProgram filename input
    compiled <- compile defs prog
    extractor compiled

prettyProgram :: String -> IO ()
prettyProgram prog = runDisplayProgram (putStrLn . pretty) "input" prog interp

runProgram :: Show a => String -> Interp a -> IO ()
runProgram = runDisplayProgram print "input"

runFile :: Show a => String -> Interp a -> IO ()
runFile filename extractor = do
  input <- readFile filename
  runDisplayProgram print filename input extractor
```

The outer `do` in `runDisplayProgram` is for the `IO` monad. The inner one is for `Either`. To run the whole pipeline, first grab the base definitions while in `IO`, then drop into `Either`. Grab the base definitions, then parse the input, then compile the base definitions and the program together, and finally run the interpreter.

My original goal when writing this was to run the [collatz sequence](https://xkcd.com/710/).

```racket
(define (step n)
  (if (even? n)
      (/ n 2)
      (+ (* n 3) 1)))

(define (collatz n)
  (if (<= n 1)
      0
      (+ 1 (collatz (step n)))))

(map collatz (range 1 15))
```

Save that to resources/collatz.lc, then run with

```
λ> runFile "resources/collatz.lc" (extractList intExtractor)
[0,1,7,2,5,8,16,3,19,6,14,9,9,17]
```

Tada! Those are the stopping times for the numbers 1 to 15.
