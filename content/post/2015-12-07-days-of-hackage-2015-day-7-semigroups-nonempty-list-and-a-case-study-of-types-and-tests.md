---
categories:
- Haskell
- Hackage
- semigroups
- nonempty lists
- null
- types
- parsec
- monoids
- algebra
- split
- safe
comments: true
date: 2015-12-07T08:00:00-05:00
layout: post
title: "24 days of Hackage, 2015: day 7: semigroups; NonEmpty list and
a case study of types and tests"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 7

How often has the following runtime error happened to you, whether in
Haskell or in some other language?

{{< highlight console >}}
*** Exception: Prelude.head: empty list
{{< /highlight >}}

Basically, code blew up that assumed a list was nonempty but it
wasn't.

In fact,
[posted on Reddit recently](https://www.reddit.com/r/haskell/comments/3vlb8v/reading_data_problems/)
was a question about code that failed with

{{< highlight console >}}
** Exception: Prelude.foldl1: empty list
{{< /highlight >}}

which is the same problem.

Today, I present a case study: refactoring that code to take advantage
of an important data type that is going to be part of the Haskell
standard library, the `NonEmpty` list type. This is part of Edward
Kmett's [`semigroups`](http://hackage.haskell.org/package/semigroups)
package that is
[going into the standard library](https://prime.haskell.org/wiki/Libraries/Proposals/SemigroupMonoid).

<!--more-->

## In general, should we adopt defensive or confident programming?

It's easy to say, "well, we should program *defensively* and not call
unsafe functions like `head` and `foldl1` but should always check for an empty
list". But what if we *know* that the list is supposed to have at
least one element, perhaps because we validated that fact earlier on
in our data model. For example, say you are buying tickets for a
concert. You have to buy at least one, but could buy more than one. So
the business logic for the ticket pickup should always assume that
there is at least one ticket for you. It should not perpetually check
for emptiness because that should have been caught up front when you
ordered.

The situation is exactly analogous to the problem of `NULL` in many
languages: languages that use `NULL` don't distinguish at the *type*
level between something that is *possibly-nonexistent* (0 or 1
element) and something that is always there (1 element). It is
unfortunate when we know something about our data but aren't saying it
in our type. In the case of lists, the situation is that a
*possibly-empty* list can have 0 or more elements, while a nonempty
list can have 1 or more elements. Both situations are extremely common
(think of regex matching that distinguishes between `x*` and `x+`) and
should be modeled by different types!

I believe that the real solution to this kind of problem is not to go
*defensive* and litter all our code with random `NULL` or `isEmpty`
kinds of runtime checks. The solution is also not to just go "cowboy
hacker" and invite possible and actual runtime exceptions by skipping
all checking.

Instead, the solution, when practical, is to *use the right types* so
that in the appropriate delimited scope of our code, runtime
exceptions *cannot possibly occur* and therefore within that scope we
can program *confidently* rather than *defensively*. If we don't use
our language's type system to our advantage, we are just doing
ordinary dynamically typed programming in a typed language and missing
out on the full benefits of types. Since we made some sacrifices in
adopting a statically typed language instead of a dynamically typed
one, we should take advantage of what we got ourselves into. There's
an interesting asymmetry in the programming world: it is *impossible*
to write statically typed code in a dynamically typed language, but it
is *easy* to write dynamically typed code in a statically typed
language!

## A nonempty list type

Let's look at the `NonEmpty` list. You can already use it today before
it becomes part of the standard library, by just adding `semigroups`
to your dependencies.

### What are the requirements for a nonempty list?

Roughly, a `NonEmpty` list should

- support operations to interconvert with a regular possibly-empty list
- support analogues of standard list operations (like `map` and
  `filter`) that carefully take into account whether the output is possibly-empty.

### QuickCheck for specifying laws (properties)

On
[day 3](/blog/2015/12/03/24-days-of-hackage-2015-day-3-hspec-the-importance-of-testing/),
I mentioned that QuickCheck is very useful for specifying requirements
when designing a new module. Ideally, when designing an API around a
new type, we treat it as an abstract data type and check the behavior
of operations on that type. The `NonEmpty` list is simple enough that you
might think it's overkill to do that, but I just wanted to present a
taste of what one might do up front if one were starting out with
property test-driven development. Let's imagine we are creating a
module `Data.List.NonEmpty`:

{{< highlight haskell >}}
{-# LANGUAGE ScopedTypeVariables #-}

-- Part of a hypothetical test module for the semigroups package.
module HypotheticalSemigroupsSpec where

import qualified Data.List.NonEmpty as NonEmpty

import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as QuickCheck
import qualified Data.Maybe as Maybe

spec :: Spec
spec =
  describe "semigroups" $ do
    describe "Data.List.NonEmpty" $ do
      describe "constructor NonEmpty.nonEmpty" $ do
        it "fails on trying to construct from an empty regular list" $ do
          NonEmpty.nonEmpty ([] :: [Int]) `shouldBe` Nothing
        prop "succeeds on any nonempty list" $ do
          \(QuickCheck.NonEmpty (xs :: [Int])) ->
            NonEmpty.nonEmpty xs `shouldSatisfy` Maybe.isJust
      describe "conversion to regular list" $ do
        prop "converts back to the original regular list" $ do
          \(QuickCheck.NonEmpty (xs :: [Int])) ->
            let Just nonEmptyXs = NonEmpty.nonEmpty xs
            in NonEmpty.toList nonEmptyXs `shouldBe` xs
{{< /highlight >}}

Basically, `nonEmpty` is a safe constructor:

{{< highlight haskell >}}
nonEmpty :: [a] -> Maybe (NonEmpty a)
{{< /highlight >}}

There is also an unsafe constructor called `fromList`. I wouldn't use
that except in case I really know that a list was not empty, because
it was returned from an API that guaranteed it but not in a typed
way. For example, I've faced this annoying problem when writing
parsers using libraries such as
[`parsec`](https://hackage.haskell.org/package/parsec), because
combinators such as `many1` have a type

{{< highlight haskell >}}
many1 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
{{< /highlight >}}

even though the library *guarantees* that if the parse succeeds, the
resulting list has at least one element! So in a situation like this,
I would feel justified in using the unsafe `NonEmpty.fromList` to
*immediately* unsafely wrap to get my known-to-be-nonempty-list into a
more refined type.

A note on QuickCheck internals: `QuickCheck.NonEmpty` (ignore the
coincidence of the token `NonEmpty` in common with our module and type
in question!) is just a
[newtype in QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck.html#t:NonEmptyList)
to generate regular lists `[a]` that happen to be nonempty at runtime
(as in they are of the form `(x:xs)`). Imagine if QuickCheck had been
written when the `NonEmpty` type we want had existed. Then it could
just generate a real `NonEmpty` rather than a fake newtype!

If you don't use QuickCheck or similar generative testing libraries,
check them out! These methods of testing are far more useful than the
example-based tests that I've provided so far in these articles just
for illustrative purposes. When possible, generative tests should be
written instead of manual example-based tests.

(The use of `[Int]` type annotations is because QuickCheck requires a
monomorphic type in order to generate concrete data to
test. `ScopedTypeVariables` is a GHC extension that I wish were just
part of the standard Haskell language; it was [covered in a 2014 Day of
GHC Extensions](https://ocharles.org.uk/blog/guest-posts/2014-12-20-scoped-type-variables.html).)

## A few notes on the full `NonEmpty` API

`map` behaves as expected, because it doesn't change the number of
elements and therefore mapping over a `NonEmpty` is clearly a `NonEmpty`:

{{< highlight haskell >}}
map :: (a -> b) -> NonEmpty a -> NonEmpty b
{{< /highlight >}}

However, `filter` on a `NonEmpty` returns a regular possibly-empty
list, as it should, because the predicate can potentially fail on
every element:

{{< highlight haskell >}}
filter :: (a -> Bool) -> NonEmpty a -> [a]
{{< /highlight >}}

And `foldl1`, unlike that of the regular list, is *safe*. It cannot
fail, because it always starts off the reduction with the first
element as the seed.

There are a whole bunch of other useful list functions.

Oh, and the [internal implementation](http://hackage.haskell.org/package/semigroups-0.18.0.1/docs/src/Data-List-NonEmpty.html#NonEmpty) is what you might suspect: it's
just a tuple of the first element with the possibly-empty tail, with a
special "cons" operator `:|` that resembles the normal list's `:`.

{{< highlight haskell >}}
data NonEmpty a = a :| [a]
{{< /highlight >}}

## A case study in refactoring

Let's look at refactoring some
[code posted on Reddit](https://www.reddit.com/r/haskell/comments/3vlb8v/reading_data_problems/)
that was throwing an exception unexpectedly at run time.

We won't go into how the code might be written in a completely
different way, but just focus on identifying and removing unsafe code
that might throw exceptions.

The code uses the excellent package
[`split`](http://hackage.haskell.org/package/split) which I am a happy
user of. I have taken the liberty of adding comments to the code and
explicit imports for presentation here.

### Original unsafe code

{{< highlight haskell >}}
import qualified Data.List.Split as Split

totalArea :: [(Int, Int, Int)] -> Int
totalArea xs = foldl (\acc x -> (acc + partialArea x)) 0 xs

partialArea :: (Int, Int, Int) -> Int
partialArea (l, w, h) = 2 * (l*w + w*h + h*l) + slack
  where areas       = [l, w, h]

        -- 'maximum' is unsafe
        smallSides  = filter (< maximum areas) areas

        -- 'foldl1' is unsafe
        slack       = foldl1 (*) smallSides

parseFile :: String -> [(Int, Int, Int)]
parseFile xs = map (splitDimensions) (breakLines xs)

breakLines :: String -> [String]
breakLines = Split.splitOn "\n"

-- | 'read' is unsafe. '(!!)' is unsafe.
splitDimensions :: String -> (Int, Int, Int)
splitDimensions xs = (item 0, item 1, item 2)
                   where item n = read ((Split.splitOn "x" xs)!!n)
{{< /highlight >}}

This crashes on `foldl1`:

{{< highlight haskell >}}
spec :: Spec
spec =
  describe "UnsafeListExample" $ do
    it "totalArea crashes for a particular input" $ do
      let contents = "1 x 1 x 1"
      evaluate (totalArea (parseFile contents)) `shouldThrow` anyException
{{< /highlight >}}

I've already identified the problems above.

We'll ignore the fact that `read` and list indexing `(!!)` are unsafe,
because that's not what is of interest here today.

But `maximum` and `foldl1` are both unsafe on lists because they crash
on empty lists.

It turns out *from manual inspection, acting as a human static
analyzer* that the use of `maximum` is OK here, because it's operating
on a 3-element list `[l, w, h]` created just before being passed to
`maximum`. But this safety is not reflected in the types.

How about `foldl1`? If you carefully think about what `filter`
returns, you can deduce that it might return an empty list, and if you
can't guarantee that it's nonempty, then the `foldl1` will crash
hard. And it does.

That was a lot of human thinking we had to do. Luckily, tests were run
that discovered the bug, but still, the code ended up posted to Reddit
asking what the bug was, so it's not trivial to find these kinds of
lurking bugs. Is there a better way?

### Refactoring to `NonEmpty` lists

Let's use `NonEmpty`. First of all, we change `totalArea` to take a
`NonEmpty` because that's what we really want.

{{< highlight haskell >}}
totalArea :: NonEmpty (Int, Int, Int) -> Int
totalArea xs = foldl (\acc x -> (acc + partialArea x)) 0 xs
{{< /highlight >}}

Note that we only had to change the type of the parameter, not any of
the code, because `foldl` has been implemented on `NonEmpty` as well
as regular lists.

Next, we change the body of `partialArea`:

{{< highlight haskell >}}
partialArea :: (Int, Int, Int) -> Int
partialArea (l, w, h) = 2 * (l*w + w*h + h*l) + slack
  where
    areas :: NonEmpty Int
    areas = NonEmpty.fromList [l, w, h]

    -- 'maximum' is safe on 'NonEmpty'
    -- But 'smallSides' can be empty because of 'NonEmpty.filter',
    -- and 'NonEmpty.fromList' is unsafe!
    smallSides :: [Int]
    smallSides = NonEmpty.filter (< maximum areas) areas

    -- unsafe!
    smallSides1 :: NonEmpty Int
    smallSides1 = NonEmpty.fromList smallSides

    -- 'foldl1' is safe on 'NonEmpty'
    slack = foldl1 (*) smallSides1
{{< /highlight >}}

We made a number of changes:

- since we're creating a nonempty list, we turn it immediately into a
`NonEmpty`
- `maximum` is defined for `NonEmpty` so we don't have to change the
  text of the code, but we know that `maximum` is *guaranteed* to be
  safe for `NonEmpty`
- `NonEmpty.filter`, as we discussed, returns a regular list, *not* a
`NonEmpty`.
- at the end, we want to safely compute `slack`, but we can't unless
we use a safe `foldl1`
- working backwards, we must convert a regular list to a `NonEmpty`,
but there's only one way to do that, and it's unsafe!

So by refining the types used in the program, we have identified the
exact point at which *we could not write the necessary code* without
resorting to `NonEmpty.fromList`. So, strictly speaking, we haven't
made a run time error turn into a compile time error, but we have
narrowed down what is going on purely mechanically by means of
following the types.

The real problem in the code here seems to be that we wanted
`smallSides` to be nonempty. The attempt to use only safe code
automatically resolved one possible source of unsafety (`maximum`) and
pointed at a more complicated situation requiring a proof obligation
on the result of the `filter`. `partialArea` could be written to avoid
the proof obligation, by not using a list type at all for `areas` to
perform the desired logic.

### Maybe the real problem is that we shouldn't use lists at all here

You may have protested at this entire `NonEmpty` exercise all this
time, because `NonEmpty` is just the wrong type for all of this work!
**There's no point in trying to use some kind of fancy type that
doesn't fit the problem.** There was no reason for the list
`[l, w, h]` to be created at all. If you're using a type and you still
run into proof obligations, often that means the type is not the right
one.

In `partialArea`, if we state the logic of what we really wanted (the
two smallest values out of three), then we can just write what we
mean:

{{< highlight haskell >}}
-- | Don't use lists at all!
bestPartialArea :: (Int, Int, Int) -> Int
bestPartialArea (l, w, h) = 2 * (l*w + w*h + h*l) + slack
  where
    (side0, side1, _) = sort3 (l, w, h)
    slack = side0 * side1
{{< /highlight >}}

where we created a utility module `Sort3`:

{{< highlight haskell >}}
module Sort3 (sort3) where

-- | Sort exactly 3 values.
sort3 :: Ord a => (a, a, a) -> (a, a, a)
sort3 (a0, a1, a2) =
  if a0 > a1
  then if a0 > a2
       then if a2 < a1
               then (a2, a1, a0)
               else (a1, a2, a0)
       else (a1, a0, a2)
  else if a1 > a2
       then if a0 > a2
            then (a2, a0, a1)
            else (a0, a2, a1)
       else (a0, a1, a2)
{{< /highlight >}}

Yes, I did look on Hoogle (as recommended in my article yesterday on
searching for utility modules), and although I did not find this exact
function, I found the logic for it embedded in a context of optimal
sorting of vectors, in the
[`Data.Vector.Algorithms.Optimal` module](https://hackage.haskell.org/package/vector-algorithms-0.7.0.1/docs/Data-Vector-Algorithms-Optimal.html)
of the
excellent
[`vector-algorithms` package](https://hackage.haskell.org/package/vector-algorithms)
that I highly recommend when working with vectors. I copied and
pasted the logic in order to work on a simple triple.

### The value of tests

QuickCheck is a great tool. Suppose we didn't want to or couldn't
refine the types in our code, for some reason. We can often still make of
testing to try to weed out easy-to-discover bugs. For example, we
could have written a sanity check test on `totalArea` by generating a
whole bunch of random input and checking that the result is what we
expect, or at least something reasonable:

{{< highlight haskell >}}
import UnsafeListExample (totalArea, parseFile)

import Test.Hspec (Spec, hspec, describe, it, shouldSatisfy, shouldThrow, anyException)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Positive(..))
import Control.Exception (evaluate)
import Text.Printf (printf)

spec :: Spec
spec =
  describe "UnsafeListExample" $ do
    it "totalArea crashes for a particular input" $ do
      let contents = "1 x 1 x 1"
      evaluate (totalArea (parseFile contents)) `shouldThrow` anyException
    prop "totalArea gives something reasonable on any triple of ints" $ do
      \(Positive (l :: Int)) (Positive (w :: Int)) (Positive (h :: Int)) ->
        let contents = printf "%d x %d x %d" l w h
        in totalArea (parseFile contents) `shouldSatisfy` (> 0)
{{< /highlight >}}

Very quickly we get a counterexample reported by QuickCheck:

{{< highlight console >}}
  1) UnsafeListExample totalArea does not crash on any triple of ints
       uncaught exception: ErrorCall (Prelude.foldl1: empty list) (after 4 tests)
       Positive {getPositive = 2}
       Positive {getPositive = 2}
       Positive {getPositive = 2}
{{< /highlight >}}

Also, you may have wondered about that complicated optimal sort of 3
values. Did I copy and paste the logic correctly? For peace of mind, I
wrote a QuickCheck test for it:

{{< highlight haskell >}}
spec :: Spec
spec =
  describe "Sort3" $ do
    prop "sort3 sorts correctly" $ do
      \(triple :: (Int, Int, Int)) ->
        let (a0', a1', a2') = Sort3.sort3 triple
        in a0' <= a1' && a1' <= a2'
{{< /highlight >}}

I love QuickCheck. What would life be without it?

### Some bonus refactorings

Some other type-oriented refactorings into `NonEmpty` as much as
possible that I won't discuss in detail because they were mostly
irrelevant here:

{{< highlight haskell >}}
parseFile :: NonEmpty Char -> NonEmpty (Int, Int, Int)
parseFile xs = NonEmpty.map (splitDimensions) (breakLines xs)

-- | We ended up not needing the fact that the input is nonempty, and
-- converted it to a regular list.
breakLines :: NonEmpty Char -> NonEmpty String
breakLines string1 = ourSplitOn "\n" (NonEmpty.toList string1)

-- | 'read' is unsafe. '(!!)' is unsafe.
splitDimensions :: String -> (Int, Int, Int)
splitDimensions xs = (item 0, item 1, item 2)
                   where item n = read ((Split.splitOn "x" xs)!!n)

-- | Using unsafe 'NonEmpty.fromList' is safe because we know
-- the result 'from Split.splitOn' is nonempty. Note that the elements
-- themselves can be empty.
ourSplitOn :: Eq a => [a] -> [a] -> NonEmpty [a]
ourSplitOn subList list = NonEmpty.fromList (Split.splitOn subList list)
{{< /highlight >}}

The most interesting thing was a tangential observation that
`Split.splitOn` always returns a nonempty list of lists. So in
principle we could wrap the result into a `NonEmpty`. I even wrote a
little passing QuickCheck test:

{{< highlight haskell >}}
spec :: Spec
spec =
  describe "split" $ do
    prop "splitOn always results in nonempty list" $ do
      \subList (list :: String) ->
        Split.splitOn subList list `shouldSatisfy` not . null
{{< /highlight >}}

Note: `split` already has a [huge set of property tests for its own
purposes](http://hub.darcs.net/byorgey/split/browse/test/). I love the
`split` library. I think it's been very well-tested. Check it out.

## A brief note on semigroups [`Semigroup`](http://hackage.haskell.org/package/semigroups-0.18.0.1/docs/Data-Semigroup.html)

A `Semigroup` is a type class representing an algebraic structure
requiring a single associative operation to be defined on it,
"append", which in this library is provided as an operator `<>`.

{{< highlight haskell >}}
import Data.Semigroup ((<>))
{{< /highlight >}}

A QuickCheck test verifying what we already know, which is that
`String` has a `Semigroup` instance, string append, and is associative as required:

{{< highlight haskell >}}
    describe "Data.Semigroup.Semigroup" $ do
      prop "<> is associative for String" $ do
        \(x :: String) y z -> (x <> y) <> z `shouldBe` x <> (y <> z)
{{< /highlight >}}

You may already use this operator `<>` in
[`Monoid`, which is already in the standard library](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Monoid.html),
but the `Monoid` type class should really be a subclass of `Semigroup`
and that's what's going to happen in a future version of Haskell
(conceptually, it should have been there all along, but Haskell was
invented 25 years ago in 1990 and `Semigroup` was apparently not
considered important enough to put into the type class hierarchy then). The difference is that a `Monoid` also requires
an identity element `mempty`.

There's no space here to say anything about why semigroups and monoids
are useful in computing. Monoids in particular have become an everyday
word in Big Data circles because of MapReduce, which based on monoids for performance.

A few Haskell oriented resources on monoids to check out:

- [On Wikibooks](https://en.wikibooks.org/wiki/Haskell/Monoids)
  (The [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell) is a great resource for Haskell in general)
- A really nice article with code ["Gaussian distributions are monoids](https://izbicki.me/blog/gausian-distributions-are-monoids)

## Conclusion

The main takeaways today: consider using `NonEmpty` when you have a
list that you know is not empty, so that you can confidently perform
operations on it without throwing an exception.  It's just one
additional Cabal dependency away! Also make sure that you actually
wanted a list, and not a tuple or fixed-size vector or something like
that. And use QuickCheck.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
