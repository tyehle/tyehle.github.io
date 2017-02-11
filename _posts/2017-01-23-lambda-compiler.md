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

       |  (<lam> (<arg> ...) <exp>)
       |  (let ((<var> <exp>) ...) <exp>)
       |  (letrec (<var> <exp>) <exp>)

       |  (<exp> <exp> ...)

<arg> ::= _ | <var>

<lam> ::= λ | lambda
```

`<arg>` is its own form because I like using `_` as an ignored argument.

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

All the desugarings implemented in [Matt Might's original post](http://matt.might.net/articles/compiling-up-to-lambda-calculus) that are missing from the compiler are there.

There are also some new definitions that make actually writing programs a bit easier.

### Useful Functions

```racket
(define (id x) x)
(define (const x _) x)
(define hang
  ((λ (u) (u u)) (λ (u) (u u))))
```

`const` will work as expected because the interpreter is lazy. `(const x hang)` won't evaluate the second argument, so it won't hang.

`hang` is used to kill the program if `head` or `tail` is called on an empty list. Might seem a bit draconian, but I didn't implement errors or exceptions cause they're hard. So too bad.


### Booleans

The only additional boolean operation is `not`.

```racket
(define (not a)
  (a #f #t))
```


### Numbers

All things are done using [Church Numerals](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals).

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

`(div1 2 3) = 0` | as expected
`(div1 3 3) = 0` | should be 1
`(div1 4 3) = 1` | as expected

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

(define (map f xs)
  (if (null? xs)
      xs
      (cons (f (head xs)) (map f (tail xs)))))
```

Folds and maps work here like they do in every other language.

The lazy interpreter allows the right fold to exit early.

`(foldr (λ (e _) #t) #f (from 0))` will terminate, but the left fold version will not.

```racket
(define (range low high)
  (if (>= low high)
      empty
      (cons low
            (range (+ 1 low)
                   high))))
```

This definition is just more efficient than `(take (- high low) (from low))` because church numerals suck.


--------


Compiler
--------

The implementation of the compiler is the same except for the additional components.

The y-combinator seems like magic to me, but I found [this answer](https://cs.stackexchange.com/a/9651) made a lot of sense.

-----------

Interpreter
-----------


---------

Extractor
---------
