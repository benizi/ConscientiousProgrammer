---
layout: post
title: "Understanding Clojure transducers through types"
date: 2014-08-07 22:58:16 -0400
comments: true
categories: 
- Clojure
- transducers
- types
- higher-rank types
- Haskell
- monads
- type classes
- Scala
- Rich Hickey
- John Launchbury
- Simon Peyton Jones
---
Yesterday, Rich Hickey published a blog post, ["Transducers are Coming"](http://blog.cognitect.com/blog/2014/8/6/transducers-are-coming), which attracted a lot of attention.

I have a confession to make, which I have made before: I find it very difficult to understand ideas or code not presented with *types*. So I decided that the only way I could possibly understand what "transducers" are would be to actually implement them in a typed language. I ended up doing so and am sharing my findings here.

<!--more-->

## Vague types in the original blog post

Rich informally gave some type signatures in his blog post:

```text
;;reducing function signature
whatever, input -> whatever

;;transducer signature
(whatever, input -> whatever) -> (whatever, input -> whatever)
```

This was, unfortunately, not very helpful. It is hard to make sense of this pseudo-notation for types. What is quantified over what? And what is bound to what? I'll explain later what I mean by these questions.

## First discussion thread I saw

There was much tweeting online about transducers after Rich Hickey's initial announcement; the tweets did not help me, except for links posted to discussion elsewhere.

One of them was [on Hacker News](https://news.ycombinator.com/item?id=8143905). I browsed through it but found it mostly not useful. The problem was that although a lot of interesting Haskell code was thrown around, it tended to be *related* to transducers but not an *exact* translation of the concept. I already had my own intuitions about transducers being related to well-known types such as [foldables](http://www.haskell.org/haskellwiki/Foldable_and_Traversable), [iteratees](http://en.wikipedia.org/wiki/Iteratee), [lenses](https://lens.github.io/), etc. That "ordinary function composition" was involved immediately suggested the connections, because function composition is huge in these existing Haskell libraries.

But what I wanted was to understand transducers *as they are*, before even thinking about generalizations and comparisons.

### What are the types?

Rich Hickey [informally offered some types](https://news.ycombinator.com/item?id=8144385) (which he said were "a la Haskell") to try to help out:

```text
    ;;reducing fn
    x->a->x

    ;;transducer fn
    (x->a->x)->(x->b->x)
```

OK, by using type variables `a`, `b`, and `x`, that indicates what is bound to what. The blog post should have used this notation rather than

```text
;;transducer signature
(whatever, input -> whatever) -> (whatever, input -> whatever)
```

### Sample Clojure code

He also posted some sample Clojure code:

{% gist b5aefa622180681e1c81 %}

## Second discussion thread I saw

Then today, I saw a discussion thread on Reddit, titled ["Clojure's Transducers are Perverse Lenses"](http://www.reddit.com/r/haskell/comments/2cv6l4/clojures_transducers_are_perverse_lenses/).

### Actual runnable Haskell code

Rich finally posted some actual [type-checked, runnable Haskell code](http://www.reddit.com/r/haskell/comments/2cv6l4/clojures_transducers_are_perverse_lenses/)!

```haskell
-- Transducers in Haskell

mapping :: (a -> b) -> (r -> b -> r) -> (r -> a -> r)
-- Original was (b -> a) -> (r -> a -> r) -> (r -> b -> r)
-- but Michael O'Keefe in comment pointed out this is misleading
mapping f xf r a = xf r (f a)

filtering :: (a -> Bool) -> (r -> a -> r) -> (r -> a -> r)
filtering p xf r a = if p a then xf r a else r

flatmapping :: (a -> [b]) -> (r -> b -> r) -> (r -> a -> r)
flatmapping f xf r a = foldl xf r (f a)

-- for exposition only, yes, conj is gross for lazy lists
-- in Clojure conj and left folds dominate
conj xs x = xs ++ [x]
xlist xf = foldl (xf conj) []

-- build any old list function with its transducer, all the same way
xmap :: (a -> b) -> [a] -> [b]
xmap f = xlist $ mapping f 

xfilter :: (a -> Bool) -> [a] -> [a]
xfilter p = xlist $ filtering p

xflatmap :: (a -> [b]) -> [a] -> [b]
xflatmap f = xlist $ flatmapping f

-- again, not interesting for lists, but the same transform 
-- can be put to use wherever there's a step fn

xform :: (r -> Integer -> r) -> (r -> Integer -> r)
xform = mapping (+ 1) . filtering even . flatmapping (\x -> [0 .. x])


print $ xlist xform [1..5]
-- [0,1,2,0,1,2,3,4,0,1,2,3,4,5,6]
```

After this post, I knew it would not take me long to figure out transducers.

## Refactoring his Haskell code

Two things to notice about the original code:

- It has long, low-level function types rather than types that actually *name* the concepts being discussed (*reducers* and *transducers*).
- It uses hardcoded list types `[a]`.

### Type synonyms and higher-rank types

Defining lots and lots of types (whether synonyms or [newtypes](http://www.haskell.org/haskellwiki/Newtype) is standard practice when programming in a modern typed language. OK, so I defined a type synonym 

```haskell
-- Left reduce
type Reducer a r = r -> a -> r
```

But what about transducer? This is trickier.

An *invalid* attempt at a type would be

```haskell
-- Illegal!
type Transducer a b = Reducer a r -> Reducer b r
```

because the type variable `r` is not bound in the type definition. And it would be incorrect to just randomly add `r` on the left hand side as an extra parameter to the `Transducer` type, because in fact it is *critical* that a transducer *does not care* about the underlying reducer's return type `r`. How do we write the desired type?

It turns out you need [higher-rank types](http://www.haskell.org/haskellwiki/Rank-N_types). Rank-1 types are not sufficient; we need a rank-2 type to quantify `r`, to say that a transducer from `a` to `b` is a transformation that takes a reducer to a specific `r` and returns another reducer to the *same* `r`.

```haskell
-- Here's where the rank-2 type is needed
type Transducer a b = forall r . Reducer a r -> Reducer b r
```

Now we can see more clearly some *completely generic* ways to create a transducer:

```haskell
mapping :: (a -> b) -> Transducer b a
mapping f xf r a = xf r (f a)

filtering :: (a -> Bool) -> Transducer a a
filtering p xf r a = if p a then xf r a else r
```

#### A bit of history

Higher-rank types are a powerful technique for expressing "hiding" of unnecessary details about types going on somewhere. My first recollection of the real world use of rank-2 types is from 1994 (the year I started using Haskell, although I did not actually use it in my work as a software engineer until 1995), when I was excited to read a paper by John Launchbury and Simon Peyton Jones that solved, using a rank-2 type, a specific, important, practical problem, ["Lazy Functional State Threads"](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.50.3299); twenty years later, their [ST monad](http://hackage.haskell.org/package/base-4.7.0.1/docs/Control-Monad-ST.html) is still part of the standard library!

### Introducing type classes

Clojure uses [protocols](http://clojure.org/protocols) as an abstraction mechanism, and the "magic" of transducers uses protocols. In Haskell, type classes are the major abstraction mechanism (this is true of Scala also).

So I abstracted away from the hardcoded list-oriented functions and values in Rich Hickey's Haskell code:

- `foldl` abstracted to a `class Foldable`
- `conj` and empty list `[]` abstracted to a `class Conjable`

```haskell
-- Left fold
class Foldable f where
  fold :: Transducer a (f a)

class Conjable f where
  empty :: f a
  conj :: Reducer a (f a)
```

Note our reliance on transducing and reducing from one type `a` to another, `f a`.

#### `Foldable` constraint

Unlike `mapping` and `filtering`, `flatmapping` is *not completely generic*, because it depends on something being `Foldable` (implementing a `fold`):

```haskell
flatmapping :: Foldable f => (a -> f b) -> Transducer b a
flatmapping f xf r a = fold xf r (f a)
```

#### `Conjable` constraint

Finally, here's the originally list-specific code that now depends only on `Foldable` and `Conjable`:

```haskell
-- I changed Rich Hickey's code to be more general than just list
-- but accept anything Conjable
xlist :: (Foldable f, Conjable f) => Transducer b a -> f a -> f b
xlist xf = fold (xf conj) empty

-- build any old Foldable function with its transducer, all the same way
xmap :: (Foldable f, Conjable f) => (a -> b) -> f a -> f b
xmap f = xlist $ mapping f 

xfilter :: (Foldable f, Conjable f) => (a -> Bool) -> f a -> f a
xfilter p = xlist $ filtering p

xflatmap :: (Foldable f, Conjable f) => (a -> f b) -> f a -> f b
xflatmap f = xlist $ flatmapping f
```

### List-specific stuff

Here is the list-specific code:

```haskell
-- Stuff specialized to lists.
-- To use another type, just make it a Foldable and Conjable.
instance Foldable [] where
  fold = foldl

-- for exposition only, yes, conj is gross for lazy lists
-- in Clojure conj and left folds dominate
instance Conjable [] where
  empty = []
  conj xs x = xs ++ [x]

-- Note: the type does not say anything about Foldable or Conjable,
-- even though the implementation just happens to use a list!
xform :: Transducer Integer Integer
xform = mapping (+ 1) . filtering even . flatmapping (\x -> [0 .. x])

-- Again, this can munge anything Foldable and Conjable, not just a list.
munge :: (Foldable f, Conjable f) => f Integer -> f Integer
munge = xlist xform
```

Notice some very important properties of this code:

- `xform` has a type that does not mention lists at all, even though it is implemented using a list and cannot compile without the list `instance` implementations.
- `munge` also does not mention lists, and can transform anything that is `Foldable` and `Conjable`.

Example:

```haskell
-- munge a list
-- [0,1,2,0,1,2,3,4,0,1,2,3,4,5,6]
example1 :: [Integer]
example1 = munge [1..5]
```

### Implementing another type to illustrate the genericity of transducers

To illustrate Rich Hickey's main point, I implemented instances of `Foldable` and `Conjable` for a standard Haskell `Vector` library as an alternate "collection-like" type.

```haskell
-- For example using Vector instead of list
import qualified Data.Vector as V

-- Implement Foldable, Conjable type classes for Vector
instance Foldable V.Vector where
  fold = V.foldl

instance Conjable V.Vector where
  empty = V.empty
  conj = V.snoc
```

And we can run `munge` directly on a vector instead of a list, *without making any changes*:

```haskell
-- return a vector rather than a list; note the fact that munge actually
-- internally uses a list
example2 :: V.Vector Integer
example2 = munge $ V.enumFromN 1 5
```

This is *code reuse* at its best.

Note that there is nothing that ties transducers to any concrete "collection" type. We could write instances of `Foldable` and `Conjable` for some kind of "channel" abstraction, for example, and instantaneously be able to munge data coming from it and to another. In fact, this is already what is done in the real world, where Haskell and Scala are used in production at places like Facebook and Twitter to efficiently handle large amounts of data.

## My code repository

My complete code is available [on GitHub](https://github.com/FranklinChen/clojure-transducers-in-haskell).

## Conclusion

It was pretty exciting to see the announcement of the transducers library for Clojure, because it represents a level of abstraction that I think has not been expressed much in the world of dynamically typed languages, although the techniques are two decades old in the Haskell community in a statically typed setting. And I hope that I was able to convey the sheer elegance of Haskell as a way to express interesting types with practical ramifications for abstraction and pluggability.
