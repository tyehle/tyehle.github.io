---
layout: post
title: Tail Call Optimized Memoization in Scala
permalink: /memoization
---

Memoization is a technique where one saves the result of a function so if the function is ever called again with the same inputs it does not have to compute the result a second time.
The example I will use here is the Fibonacci sequence. The obvious recursive implementation given below has comically terrible performance.

```scala
def fib(n: Int):Long = {
  case _ if n < 0 =>
    throw new Exception("negative input")
  case 0 => 1
  case 1 => 1
  case _ => fib(n-1) + fib(n-2)
}
```

I like this definition. I think it is easier to understand than an optimized version with loops and lists.
Unfortunately this definition is unusably slow. Computing the value of the 10th Fibonacci number using this implementation requires 177 calls to `fib`! Many of these calls are calling `fib` with the same arguments. `fib(1)` was called 55 times.

Bummer, but memoization can save it! Since `fib` is a pure function, calls with the same arguments will always return the same value, so there is no need to compute the result again. The implementation below uses a map to store the results of previous computations.

```scala
val cache = mutable.Map.empty[Int, Long]

def memoizedFib(n: Int): Long = {
  cache.get(n) match {
    case Some(answer) => answer
    case None =>
      val answer = n match {
        case _ if n < 0 =>
          throw new Exception("negative input")
        case 0 => 1
        case 1 => 1
        case _ => memoizedFib(n-1) + memoizedFib(n-2)
      }
      cache.update(n, answer)
      answer
  }
}
```

This version has the desired linear performance, requiring only 11 calls to fib to compute the value of the 10th number in the sequence.
Great, but that code is ugly and no fun to write.

***

Generalizing to Any Function
----------------------------

What I want is a small change I can make to the original function to get the power of memoization.
The first step is to make a more general `memoize` function.

Due to limitations in scala generics the functions I will be able to memoize will all have the type signature `I=>O`, that is they accept only one input of type `I` and produce an output of type `O`.

If I am going to write a more general `memoize` function, in what way should it be more general? I decided that forcing a specific caching system was not necessisary.
Checking a cache may be able to give an answer very quickly, but not always, but may sometimes fail. I will call this function `check` and its type is `I=>Option[O]`.
After I compute a result the hard way I should save it away so `check` can return it in the future. I will call this saving function `update` and its type is `(I,O)=>Unit`. Using this framework I implemented a generic `memoize` function.

```scala
def memoize[I, O](fn: I => O,
                  check: I => Option[O],
                  update: (I, O) => Unit): I => O = {
  (input: I) => check(input) match {
    case Some(answer) => answer
    case None =>
      val answer = fn(input)
      update(input, answer)
      answer
  }
}
```

Cool! I also implemented a `memoizeMap` function that memoizes some function, `I=>O`, using a map for convenience.

```scala
def memoizeMap[I, O](fn: I => O): I => O = {
  val cache = mutable.Map.empty[I, O]
  memoize(fn, cache.get, cache.update)
}
```

Now I can finally get the syntax I want. Unfortunately `def` is somewhat special in scala, so the best syntax I could wrangle is the following.

```scala
lazy val fib: Int => Long = memoizeMap {
  case n if n < 0 =>
    throw new Exception("negative input")
  case 0 => 1
  case 1 => 1
  case n => fib(n-1) + fib(n-2)
}
```

The `lazy` keyword is the only way I could figure out to get recursive binding without using `def`.

***

Tail Recursion
--------------

What if I want to compute the 100 000th Fibonacci number though? Things get a little more tricky.
The syntax I ended up with is not very readable, so iteration is probably a more useful (and faster) solution for most functions.
There are many functions that are not easily represented as iteration for which the method I use here is useful.

First of all: what is tail recursion? A tail recursive function only has calls to itself in tail position.
In a language like Java with an explicit `return` keyword this means the only recursive calls are of the form `return myself(...);`.
More generally, the result of the recursive function is equal to the recursive call, or some atomic value.

The Fibonacci function branches into two subcalls, `fib(n-1)` and `fib(n-2)`, at every step which introduces its own set of problems.
For starters  I implemented factorial.

```scala
lazy val fact: Int => BigInt = memoizeMap {
  case n if n < 0 =>
    throw new Exception("negative input")
  case 0 => 1
  case n => n * fact(n - 1)
}
```

This implementation is not tail recursive. Notice the value of a call to `fact` is `n * fact(...)`. The traditional way to make this function tail recursive is include an accumulator as an argument to the function. Unfortunately this will make the memoization useless because every intermediate value is unique to the original call.

***

Continuation Passing Style
--------------------------

I will leave a full explanation of continuation passing style to others.
Some good explinations are [here](http://matt.might.net/articles/by-example-continuation-passing-style/) and [here](https://www.cs.utah.edu/~mflatt/past-courses/cs6520/public_html/s02/cps.pdf).
In this specific case the continuation acts as a return for the recursive function. The recursive function must, at some point, call the continuation it is passed with the result it is to return.
Note the type of the continuation here is `O=>O`. The following is the same factorial function rewritten in continuation passing style. This change lets me write all recursive calls in tail position.

```scala
def cpsFact(input: Int, kont: BigInt=>BigInt): BigInt = {
  input match {
    case n if n < 0 =>
      throw new Exception("negative input")
    case 0 => kont(1)
    case n => cpsFact(n-1,
                      (result: BigInt) => kont(n * result))
  }
}
```

I wrote a new memoizing function that accepts CPS functions.
Unlike the first one this memoizer is properly tail recursive.
The update step is moved into the continuation of the recursive call, ensuring the stack frame of the lambda can be torn down.

```scala
def memoizeRec[I, O](fn: (I, O=>O) => O,
                     check: I => Option[O],
                     update: (I, O) => Unit):
                    (I, O=>O) => O = {
  (input: I, kont: O=>O) => check(input) match {
    case Some(answer) => kont(answer)
    case None => fn(input,
                    (result: O) => {
                      update(input, result)
                      kont(result)
                    })
  }
}
```

Unfortunately the scala compiler will not tail call optimize lazy vals, so I needed a different solution.
The scala standard library had all I needed to fix the problem tucked away in the [TailCalls](http://www.scala-lang.org/api/2.12.0-M4/scala/util/control/TailCalls$.html) library.
The implementation containing the tail call optimization is given below.

```scala
def memoizeRec[I, O](fn: (I, O=>TailRec[O]) => TailRec[O],
                     check: I => Option[O],
                     update: (I, O) => Unit):
                    (I, O=>TailRec[O]) => TailRec[O] = {
  (input: I, kont: O=>TailRec[O]) => check(input) match {
    case Some(answer) => tailcall(kont(answer))
    case None =>
      tailcall(fn(input, (result: O) => {
        update(input, result)
        tailcall(kont(result))
      }))
  }
}

def memoizeRecMap[I, O](fn: (I, O=>TailRec[O]) => TailRec[O]):
    (I, O=>TailRec[O]) => TailRec[O] = {
  val cache = mutable.Map.empty[I, O]
  memoizeRec(fn, cache.get, cache.update)
}

lazy val cpsFactTail: (Int, BigInt=>TailRec[BigInt]) =>
                      TailRec[BigInt] = memoizeRecMap {
  (input, kont) => input match {
    case n if n < 0 =>
      throw new Exception("negative input")
    case 0 => tailcall(kont(1))
    case n =>
      tailcall(cpsFactTail(n-1,
        (result: BigInt) => tailcall(kont(n * result))))
  }
}
```

I said earlier that the Fibonacci sequence was harder to express using only tail calls.
You are only allowed to make a single recursive call, so the continuation must contain the other recursive call.
Remember continuations must also only contain tail calls, so the continuation passed to the recursive call will be a tail recursive call with a final continuation calling the outer continuation with the final result of this function call. Not very intuitive, I know.

```scala
lazy val cpsFibTail: (Int, BigInt=>TailRec[BigInt]) =>
                     TailRec[BigInt] = memoizeRecMap {
  (input, kont) => input match {
    case n if n < 0 =>
      throw new Exception("negative input")
    case 0 => tailcall(kont(1))
    case 1 => tailcall(kont(1))
    case n => tailcall(cpsFibTail(n-1, (res1: BigInt) =>
      tailcall(cpsFibTail(n-2, (res2: BigInt) =>
        tailcall(kont(res1 + res2))))))
  }
}
```

With this implementation I was able to compute the 100 000th Fibonacci number in just 2.2 seconds.

***

Limitations
-----------

- Functions must be pure. Memoizing Random.nextInt is not very useful.

- Continuations are not easy to read. All of the simplicity of the initial Fibonacci function has been lost. In most cases arbitrary depth recursion is not a requirement, so the more elegant `memoizeMap` version could be used instead.

- Input must be a single type. Without macros accepting an arbitrary number inputs in a typesafe manner is simply not possible. Yes you could write a different version that accepts two input functions, or put your arguments into a tuple. In my mind these are not acceptable solutions.