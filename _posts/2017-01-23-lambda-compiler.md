---
layout: post
title: Compiling and Interpreting the Lambda Calculus in Haskell
published: false
---

This whole project is based on [this post by Matt Might](http://matt.might.net/articles/compiling-up-to-lambda-calculus). I decided I would try to implement it in Haskell instead of Racket. There are several steps of the process that are missed because they are built into the Racket language. In my implementation I needed a parser, an interpreter, and an extractor, in addition to a reimplementation of the compiler.

Instead of defining all the functions in the language in the compiler, I thought it would be more fun to write them in the language itself. This greatly reduces the complexity of the parser and the compiler. Also it means you can add any library functions you see fit, and I ended up adding some comparisons, and list operations.

All code for the project can be found [on github](https://github.com/tyehle/lambda).


------


Parser
------
Because many functions are implemented in the language itself, the grammar is much smaller.

```
<prg> ::= <def> ... <exp>

<mod> ::= <def> ...

<def> ::= (define <var> <exp>)
       |  (define (<var> <arg> ...) <exp>)

<exp> ::= <var>

       |  <nat>

       |  ([λ|lambda] (<arg> ...) <exp>)
       |  (let ((<var> <exp>) ...) <exp>)
       |  (letrec (<var> <exp>) <exp>)

       |  (<exp> <exp> ...)

<arg> ::= _ | <var>
```

`<arg>` its own form because I like using `_` as an ignored argument.

I added program, module and define forms to make it possible to do bootstrapping.



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

This will result in a `Left` with the shown version of the `ParseError` if the parser fails.

Every stage of the compiler has the type `a -> Either String b` so it can all be strung together with bind.


--------


Base Definitions
----------------

All the definitions of basic functions in the language are in [resources/base.lc](https://github.com/tyehle/lambda/blob/master/resources/base.lc)

Most functions are the same as the desugarings implemented in [Matt Might's original post](http://matt.might.net/articles/compiling-up-to-lambda-calculus).

There are some new ones

### Basic Functions
```racket
(define (id x) x)
(define (const x _) x)
(define hang
  ((λ (u) (u u)) (λ (u) (u u))))
```

`const` will work as expected because the interpreter is lazy. `(const x hang)` won't evaluate the second argument, so it won't hang.

`hang` is used to kill the program if `head` or `tail` is called on an empty list. Might seem a bit draconian, but I didn't implement errors or exceptions cause they're hard. So too bad.


--------


Compiler
--------

The implementation of the compiler is the same except for the additional components.

The y-combinator seems like magic to me, but I found [this answer](https://cs.stackexchange.com/a/9651) made a lot of sense.


### Not

`not` is easy to implement using `if`.

```haskell
compile (Not a) = compile $ If a VFalse VTrue
```


### Even

I decided to make an `even?` function be its own syntactic form because there is an efficient implementation available for church numerals.

A church numeral representing the number n is a function that takes some operation `f` and an argument `x` and returns `f` applied n times to `x`.

We can use this definition to write `even?` as `(lambda (n) (n not #t))`.

In the compiler this looks like this

```haskell
compile (IsEven a) = compile a `App` compile switch `App` true
  where
    switch = Lambda ["x"] $ If (Var "x") VFalse VTrue
```

You may have noticed this will take at least linear time as opposed to constant time one would expect from a real programming language. This is because church numerals are slow and should never be used unless you like [office chair battles](https://xkcd.com/303/).


### Division

The algorithm I used for division is repeated subtraction, and is described [on wikipedia](https://en.wikipedia.org/wiki/Church_encoding#Division).



```haskell
divide :: Node
divide = Lam "n" $ Lam "m" $ compile (div1let (Var "n") (Var "m"))
  where
    -- (if (zero? diff) 0 (+ 1 (div1 diff m)))
    body = If (IsZero (Var "diff"))
              (Num 0)
              (Plus (Num 1)
                    (Var "div1" `Application` Var "diff" `Application` Var "m"))
    -- (let ([diff (- n m)]) body)
    minusBinding = Let [("diff", Minus (Var "n") (Var "m"))] body
    div1let n m = Letrec ("div1", (["n", "m"], minusBinding))
                         (Var "div1" `Application` Plus (Num 1) n `Application` m)
```

-----------

Interpreter
-----------


---------

Extractor
---------
