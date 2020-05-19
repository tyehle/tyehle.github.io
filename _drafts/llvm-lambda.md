---
layout: post
title: Compiling Lambdas Using LLVM
---


A very basic language:

```Haskell
data Expr = Nat Int
          | Plus Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | Divide Expr Expr
          | Ref String
          | Let String Expr Expr
          | Lambda [String] Expr
          | App Expr [Expr]
          deriving (Eq, Show)
```

We will compile this using the haskell llvm bindings.
