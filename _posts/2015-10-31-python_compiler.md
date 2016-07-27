---
layout: post
title: Python Compiler
permalink: /articles/python-compiler
---

Every assignment in [Matt Might's compilers class](http://matt.might.net/teaching/compilers/spring-2015/) at the University of Utah is a piece of a compiler for Python. In an ideal world at the end of the semester all of the students would have made an end to end system that can fully compile Python into a low level language such as Java bytecode or x86. During the semester Matt Might was called to Washington to help set up the [Precision Medicine Initiative](http://www.nih.gov/precisionmedicine/) which caused the class schedule to lag. There were only four projects in the end, and the output of the system was a normalized subset of Python ready for a CPS transform.

***

Lexing
------

This project took an file of valid Python code and produced a sequence of tokens. The tokens were one of eight s-expressions:

```
(NEWLINE)

(INDENT)

(DEDENT)

(ID name)

(LIT value)

(KEYWORD symbol)

(PUNCT text)

(ENDMARKER)
```

For example if the lexer got the input

```python
def f(x):
    return 7*x
```

It would generate

```
(KEYWORD def)
(ID "f")
(PUNCT "(")
(ID "x")
(PUNCT ")")
(PUNCT ":")
(NEWLINE)
(INDENT)
(KEYWORD return)
(LIT 7)
(PUNCT "*")
(ID "x")
(NEWLINE)
(DEDENT)
(ENDMARKER)
```

The hardest part of the stage was dealing with unicode in variable names, and strings. Variables were required to be NFKC normalized, and string literals had to be properly represented in Racket. The lexer requires some state to deal with the whitespace correctly, and there are some interesting corner cases with the ellipsis literal and a regular period.

***

Parsing
-------

The parser we built was really just a bunch of reduction rules on top of the racket parsing library and DERP2 built by Matt Might (if you want to know more about how this works, the [code is available](https://github.com/mattmight/project-python-parser/blob/master/sxgram2yaccgram.rkt), and there is also [this fantastic post](http://theorangeduck.com/page/you-could-have-invented-parser-combinators) about how parser combinators work).

We started with the [Python grammar specification](https://docs.python.org/3/reference/grammar.html), and inserted reductions to give us the parse tree matching [Python's built in AST module](https://docs.python.org/3/library/ast.html#abstract-grammar). As an example of what this entails I will walk through the process for dealing with an if statement.

From the Python grammar, an if statement looks like this (shown here in lisp-like syntax):

```racket
(if_stmt (seq "if"
              test
              ":"
              suite
              (rep (seq "elif" test ":" suite))
              (opt (seq "else" ":" suite))))
```

So an if statement must have a test expression and a body, and then any number of else if clauses, followed by an optional else clause.

In my code I use the function `($--> input output)`, which generates a reduction rule that matches the first argument and results in the second argument. Inside the reduction, parts of the input can be extracted with the `($ n)` function, and the symbol `$$` gets expanded to the entire input.

An example of a simple reduction is the following application.

```racket
($--> (seq "if" test ":" suite)

      (list ($ 2) ($ 4)))
```

This will match the literal `"if"`, any expansion of the non-terminal `test`, the literal `":"`, and finally any expansion of the non-terminal `suite`. It will return a list of just the second and fourth things it matched, in this case the expansion of `test` and `suite`.

The following is the full set of reductions for an if statement.

```racket
(if_stmt ($--> (seq ($--> (seq "if" test ":" suite)
                          (list ($ 2) ($ 4)))
                    (rep ($--> (seq "elif" test ":" suite)
                               (list ($ 2) ($ 4))))
                    ($--> (opt (seq "else" ":" suite))
                          (match $$
                            [#f '()]
                            [`("else" ":" ,block) block])))
               (process-ifs (cons ($ 1) ($ 2)) ($ 3))))
```

The inner reductions filter out the unneeded keywords, and the outer reduction is a simple call to a helper function that builds the correct output syntax. The helper function takes a list of if/elif statements, and a possibly empty else statement. It recursively packages them up into the correct if-else syntax.

```racket
(define (process-ifs ifs tail)
  (match ifs
    [`((,test ,body))
     `(If (test ,test)
          (body ,@body)
          (orelse ,@tail))]
    [(cons `(,test ,body) others)
     `(If (test ,test)
          (body ,@body)
          (orelse ,(process-ifs others tail)))]))
```

The final system would take the following if statement

```python
if t1:
  r1
elif t2:
  r2
else:
  r3
```

and produce this syntax tree

```racket
(If
 (test (Name t1))
 (body (Expr (Name r1)))
 (orelse (If (test (Name t2))
             (body (Expr (Name r2)))
             (orelse (Expr (Name r3))))))
```

***

Desugaring
----------

The next two projects were desugaring and normalization steps. They both used a [tree walker](https://github.com/mattmight/pywalk) built for the output of the parser. The implementation of the tree walker simply does a match on a structure in the parse tree, and recursively descends until it hits a leaf, performing a transformation along the way. The interface provided by the tree walker is the ability to walk the tree performing some transformation on every subtree. It also provided the functions to perform a transformation until a fixed point was hit, and the option to transform top down or bottom up.

Basically all of the code that I wrote for this part was transformation functions that take a statement, and return a list of possible replacement statements. The most basic transform is fixing return statements so that a lone `return` call becomes `return None`.

```racket
(define (canonicalize-return stmt)
  (match stmt
    [`(Return)  (list '(Return (NameConstant None)))]
    [else       (list stmt)]))
```

Some of the other interesting transforms we did in the phase are lifting decorators, and removing classes. Removing decorators from functions is a straightforward operation. Generally code of the form

```
@<decorator>
def <name>(<parameters>):
  <body>
```

should be turned into code of the form

```
def <name>(<parameters>):
  <body>
<name> = <decorator>(<name>)
```

After the end of this phase of the compiler we needed to normalize before applying any more desugarings.

***

Normalizing
-----------

The main goal of normalization is breaking arbitrarily complex statements into bits that can be easily translated to machine code. This step is also where the order of evaluation is fixed. For example after this phase the statement `f(print(1), print(2))` could not possibly do anything other than print `1` then print `2`. In order to ensure the entire program was normalized we ran a normalizing tree transform until nothing happened. The normalizing transform matched every complex statement in the grammar and checked if all its sub-expressions were atomic. If they were not it bound them to a variable and then made the complex call with the variable instead.

For example the statement

```python
result = 3*x + 4*y
```

would be interally represented after parsing and desugaring as

```racket
'(Assign (targets (Name result))
         (BinOp (BinOp (Num 3) Mult (Name x))
                Add
                (BinOp (Num 4) Mult (Name y))))
```

This internal representation would trigger the following match clause in the body of `normalize/stmt`.

```racket
[`(Assign (targets ,target) (value ,rhs))
 #:when (not (normalized? rhs))
 `((Assign (targets ,target)
           (value ,(normalize-expr! rhs))))]
```

Then the following match clause would do the real work in the body of `normalize-expr!`.

``racket
[`(BinOp ,lhs ,op ,rhs)
 `(BinOp ,(atomize! lhs) ,op ,(atomize! rhs))]
```

The `atomize!` function would just push the complex statements into their own assign statements and return the name of the new variable they were assigned to. The original statement would then look like this

```python
_tmp_1 = 3*x
_tmp_2 = 4*y
result = _tmp_1 + _tmp_2
```

***

Finishing
---------

There were still a couple of nasty things to take care of before the Python structures we had at this point could be translated into machine code. In order to deal with the yield construct we were going to do a double [CPS transform](http://matt.might.net/articles/cps-conversion/). There was also a fair amount of other steps to take before we arrived at a true low level language.
