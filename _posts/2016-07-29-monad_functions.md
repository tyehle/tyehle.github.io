---
layout: post
title: Monad Functions
published: false
---

If you have tried to learn about monads in Haskell, you have probably heard that all Applicatives are Functors and all Monads are Applicatives, but (at least for me) this relationship is not obvious.
It is my goal to show how defining an instance of Applicative implies the existence of a valid definition for Functor, and how an instance of Monad implies a definition for Applicative.

First, some definitions. I will use these functions as examples throughout the post.

```haskell
interact :: IO String
interact = getLine >>= \n -> if (even (read n)) then return "Nothing to report" else return "Huh, thats odd"

message :: IO ()
message = putStrLn "Good morning!"
```


***

Functor
-------

#### Required functions

- `fmap :: Functor f => (a -> b) -> f a -> f b`

#### Conditions

- `fmap id = id`
- `fmap (f . g) = fmap f . fmap g`

When we define `fmap` using the applicative functions we must show that these conditions hold.

#### Other functions

```haskell
<$ :: Functor f => a -> f b -> f a
<$ = fmap . const -- maps everything to the given value
```


***

Applicative
-----------

Required functions

pure :: Applicative f => a -> f a
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

Conditions

???

Other functions

(<*) :: Applicative f => f a -> f b -> f a
u <* v = pure const <*> u <*> v
-- we take a function with the type `f (a -> b -> a)` and apply (`<*>`) it to a value with the type `f a` to get a value with the type `f (b -> a)`. We can then apply this value to the second argument with the type `f b` to get the resulting type `f a`. Notice `u` is applied first, and then `v`.

(*>) :: Applicative f => f a -> f b -> f b
u *> v = pure (const id) <*> u <*> v

You might think that you could just do `pure const <*> v <*> u`, but this would put stateful operations in the wrong order. Try with `interact <* message`
This ends up being very similar to the first function, but instead of double applying a function with the type `f (a -> b -> a)`, we need a function with the type `f (a -> b -> b)`. We get a function with the type `a -> b -> b` by applying the identity function to `const`, and then puting that in the applicative context with `pure`. This gives us a function that takes the arguments in the correct order and keeps the right result.


Results

fmap f x = pure f <*> x
We must also show that fmap id = id and fmap (f . g) = fmap f . fmap g for this to be a valid definition.

(\x -> pure id <*> x) =?= id
This function puts `id` into the Applicative context using `pure`, and then applies it to the argument. It is indeed equal to the identity as long as `x` is an instance of Applicative.

(\x -> pure (f . g) <*> x) =?= fmap f . fmap g

This strikes me as very silly. Why do I need to prove this? I'm not sure.

Examples

Prelude> Just 3 *> Nothing
Nothing

Prelude> Just 3 <* Nothing
Nothing

Prelude> Nothing *> Just 3
Nothing

Prelude> Nothing <* Just 3
Nothing

Prelude> Just 1 *> Just 2
Just 2

Prelude> Just 1 <* Just 2
Just 1

message *> interact
interact <* message


***

Monad
-----

Required functions

return :: Monad m => a -> m a
(>>=) :: Monad m => m a -> (a -> m b) -> m b

If the instance of a monad is alread an instance of Applicative, then `return` can be defined equal to `pure`. This is not the default for reasons of backward compatibility (much existing code defines an applicative instance using a monad instance, so making this the default can lead to definition loops somehow?).

Other functions

(>>) :: Monad m => m a -> m b -> m b
a >> b = a >>= const b


Results

pure = return

u <*> v = u >>= (\f -> v >>= return . f)

This operates by first extracting the function to apply from the monad context (u >>= ... ), and then extracting the argument (... v >>= ...), applying the function and returning the result back to the monad context (... >>= return . f).

`<*>` is the same as the predefined monad function `ap`. The definition of `ap` in the Haskell source uses do syntax and is maybe a little easier to understand than the definition I have written above.
ap m1 m2 = do { x1 <- m1; x2 <- m2; return (x1 x2) }
