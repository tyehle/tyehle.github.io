---
layout: post
title: Algebraic Data Types in the Lambda Calculus
---

As I was writing a [compiler for the lambda calculus]({% post_url 2017-01-23-lambda-compiler %}), I noticed some similarities between booleans and lists. They both have two variants, and in order to use a boolean or a list value you must apply it two to functions that say what to do in either case. Like this

```racket
(some-bool
  value-if-true
  value-if-false)

(some-list
  (λ (fst rst) (do-if-cons fst rst))
  value-if-nil)
```

There is only one way to construct boolean and list values you can use this way. Everything must be a closure, so booleans and lists are closures that expect two arguments.
```racket
(define (Cons fst rst) (λ (do-cons nil-val) (do-cons fst rst)))
(define Empty          (λ (do-cons nil-val) nil-val))

(define True  (λ (t f) t))
(define False (λ (t f) f))
```

Using a function in this way seemed very similar to the `case` or `match` syntax in many languages. I wondered if I could create some syntactic sugar for algebraic data types in general for which booleans and lists would be special cases.

For example a `struct` definition

```racket
(struct Bool
  [True]
  [False])

(struct (List a)
  [Cons a (List a)]
  [Empty])
```

with a corresponding `case` keyword for destructuring

```racket
(case some-bool
  [True value-if-true]
  [False value-if-false])

(case some-list
  [(Cons fst rst) (do-if-cons fst rst)]
  [Empty value-if-nil])
```

To make this work the `struct` keyword will need to introduce constructors for the variants, and the `case` keyword will need allow us to destructure an instance of a struct.


********************

Case Keyword
------------

Lets start by looking at an example of the transform. The following function will find the depth of a tree structure.

```racket
(define (depth tree)
  (case tree
    [Empty 0]
    [(Leaf data) 1]
    [(Tree left right) (+ 1 (max (depth left) (depth right)))]))
```

Just like the boolean and list examples, it should desugar into an application of closure named `tree` that is passed into the depth function.

```racket
(define (depth tree)
  (tree
    0
    [λ (data) 1]
    [λ (left right) (+ 1 (max (depth left) (depth right)))]))
```

The bindings in the patterns become lambda parameters, and the expression becomes the body of the lambda. If the pattern we are matching against is a constant like `Empty` then there are no bound variables, and the expression is applied directly. It could also become an application of a lambda with no parameters, but the evaluation model I'm using is lazy, so there is no need to wrap the value in an additional lambda. ie: we just have `0` instead of `[λ () 0]`.

The only complication for doing this transform in general is the ordering of the case clauses. We should have been able to order the clauses `Tree`, `Leaf`, `Empty`, instead of `Empty`, `Leaf`, `Tree`, and it should have desugared to the same code. To ensure this happens we need to apply the clauses in the same order they appear in the struct definition.

In general the statement
```racket
(case <exp>
  [<C2>                      <exp2>]
  ...
  [(<Cm> <a1> <a2> ... <an>) <expm>]
  ...
  [(<C1> <a1> <a2> ... <an>) <exp1>])
```
desugars to
```racket
(<exp>
  (λ (<a1> <a2> ... <an>) <exp1>)
  <exp2>
  ...
  (λ (<a1> <a2> ... <an>) <expm>))
```


********************

Struct keyword
--------------

The definition of the tree structure we used before would look like

```racket
(struct (Tree a)
  [Empty]
  [Leaf a]
  [Node (Tree a) (Tree a)])
```

The compiler needs to keep track of the order of the variants in order to desugar any `case` statements correctly, but it also needs to generate a constructor for each variant. This tree structure has three variants, so the constructors would be:
- `Empty :: Tree a`
- `Leaf :: a -> Tree a`
- `Node :: Tree a -> Tree a -> Tree a`.

We know that the constructors need to generate a closure that expects three arguments, one for each variant. Each of those arguments will be a function that operates on the data stored in that variant.

```racket
(define Empty             (λ empty leaf node) empty)
(define (Leaf data)       (λ empty leaf node) (leaf data))
(define (Node left right) (λ empty leaf node) (node left right))
```

But why did we pick the variable names `left`, `right`, and `data`? We have some intuition about the meaning of these variables, but the desugarer will need to generate these variables names. The lambda parameters also need to be generated, and we could reuse the name of the variant, but this may cause confusion to a human, so I will mangle them. With these changes the definitions would look like this
```racket
(define Empty        (λ _Empty _Leaf _Node) _Empty)
(define (Leaf a1)    (λ _Empty _Leaf _Node) (_Leaf a1))
(define (Node a1 a2) (λ _Empty _Leaf _Node) (_Node a1 a2))
```

In general the statement

```racket
(struct <Name>
  [<C1> <T1> <T2> ... <Tn>]
  [<C2>]
  ...
  [<Cm> <T1> <T2> ... <Tn>])
```
desugars to
```racket
(define (<C1> a1 a2 ... an) (λ (<_C1> <_C2> ... <_Cm>) (<_C1> a1 a2 ... an) ))
(define <C2>                (λ (<_C1> <_C2> ... <_Cm>) <_C2>))
...
(define (<Cm> a1 a2 ... an) (λ (<_C1> <_C2> ... <_Cm>) (<_Cm> a1 a2 ... an) ))
```


********************

Examples
--------

I started this project with the goal of generalizing the syntax of booleans and lists to something that will work for any algebraic data type.

### Booleans

It is easy to see that the simple example of an if statement will desugar the same way as the standard encoding for [Church booleans](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans).

```racket
(struct Bool
  [True]
  [False])

(define (if c t f)
  (case c
    [True  t]
    [False f]))
```
becomes
```racket
(define True  (λ (_True_0 _False_0) _True_0))
(define False (λ (_True_0 _False_0) _False_0))

(define (if c t f)
  (c t f))
```


### Lists

The `isNil` function compiles to exactly the same lambda expression it did before.

```racket
(struct (List a)
  [Empty]
  [Cons a (List a)])

(define (isNil list)
  (case list
    [Empty True]
    [(Cons _ _) False]))
```
becomes
```racket
(define Empty        (λ (_Cons _Empty) _Empty))
(define (Cons a1 a2) (λ (_Cons _Empty) (_Cons a1 a2)))

(define (isNil list)
  (list True (λ (_ _) False)))
```


### Maybe

It is now a breeze to define other well known functions like the safe version of `head`.

```racket
(struct (Maybe a)
  [Just a]
  [Nothing])

(struct (List a)
  [Empty]
  [Cons a (List a)])

(define (head? list)
  (case list
    [Empty             Nothing]
    [(Cons first rest) (Just first)]))
```

becomes

```racket
(define (Just a1) (λ (_Just _Nothing) (_Just a1)))
(define Nothing   (λ (_Just _Nothing) _Nothing))

(define Empty        (λ (_Cons _Empty) _Empty))
(define (Cons a1 a2) (λ (_Cons _Empty) (_Cons a1 a2)))

(define (head? list)
  (list
    Nothing
    [λ (first rest) (Just first)]))
```
