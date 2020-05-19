---
layout: post
title: Writing A Simple Garbage Collector
---

I wrote a [compiler for a small language](https://github.com/tyehle/llvm-lambda) in Haskell that can build x86 binaries using [LLVM](https://llvm.org/). Its got closures, numbers with basic arithmetic, and basic conditionals. Using these basic tools it is possible to write programs to do some simple things like compute factorials,

```scheme
(let (fact (lambda (n rec)
             (if0 n
               1
               (* n (rec
                      (- n 1)
                      rec)))))
  (fact 5 fact))
```

or loop forever

```scheme
((lambda (x) (x x))
 (lambda (x) (x x)))
```

Unfortunatly there is still a missing piece. Whenever the compiler needs to generate an object, it puts it on the heap, but those objects remain there until the program finishes and all requested memory is reclaimed by the OS. The factorial program will leave a whole bunch of integers lying around on the heap, and the second program won't run forever at all! Instead, it will keep allocating new closures until the system eventually runs out of memory, and then [something else will happen](https://unix.stackexchange.com/questions/153585/how-does-the-oom-killer-decide-which-process-to-kill-first). Probably the OS will kill the program. In order to fix these problems, the compiler should be deleting old objects out of heap space to free up room for new ones.

This language needs a garbage collector.

---


Defining Success
----------------

- What is this article about?
- What does it mean to be a GC that works
- Stretch goals for a GC. How to be "good"
- ""

So what does it mean for a language to "have a garbage collector"? The language I've built isn't fast or efficient
Great, so what does that mean and how can I build it?: I need a GC, what does that mean and how to do?

I don't need a fancy one, just the simplest one will do

When I started this project, I understood the basic shape of garbage collector, but there were several questions I did not know the answers to:
- How do we know which variables are in scope?
- When and how would the garbage collector be called?
- Where should the garbage collector code go, and what language should I write it in?


What does it means for a garbage collector to "work"?

---

Required Bookeeping
-------------------

What global information does the runtime need to keep track of?

---

Taking out the Trash
--------------------




- Changing the memory layout of a closure so we can traverse it