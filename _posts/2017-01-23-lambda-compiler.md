---
layout: post
title: Compiling and Interpreting the Lambda Calculus in Haskell
published: false
---

This whole project is based on [this post by Matt Might](http://matt.might.net/articles/compiling-up-to-lambda-calculus). I decided I would try to implement it in Haskell instead of Racket. There are several steps of the process that are missed because they are built into the Racket language. In my implementation I needed a parser, an interpreter, and an extractor, in addition to a reimplementation of the compiler.

Instead of defining all the functions in the language in the compiler, I thought it would be more fun to write them in the language itself. This greatly reduces the complexity of the parser and the compiler. Also it means you can add any library functions you see fit, and I ended up adding some comparisons, and list operations.

All code for the project can be found [on github](https://github.com/tyehle/lambda).

### Sections
{% include toc.html %}


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

Church encoded lists are similar to booleans. They are functions that take a thing to do if the list is a pair, and a thing to do if the list is empty. If the list is a pair then the function is given two arguments, the head and the tail. If the list is empty then the function is called with the identity function.

```racket
(define (cons h t)
  (λ (f _) (f h t)))
(define empty
  (λ (_ e) (e id)))
```

Using these definitions we can define common list operations, some of which I have listed below.

```racket
(define (head l)
  (l (λ (h _) h) hang))
(define (tail l)
  (l (λ (_ t) t) hang))
(define (pair? l)
  (l (λ (_ _) #t) (const #f)))
(define (null? l)
  (l (λ (_ _) #f) (const #t)))
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


---------

Extractor
---------
