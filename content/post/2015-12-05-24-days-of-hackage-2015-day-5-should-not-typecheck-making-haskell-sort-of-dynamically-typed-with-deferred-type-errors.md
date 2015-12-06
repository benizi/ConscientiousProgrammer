---
categories:
- Haskell
- Hackage
- should-not-typecheck
- Stack
- dynamic
- deferred type errors
- GHC extensions
- testing
- HSpec
comments: true
date: 2015-12-05T08:20:32-05:00
layout: post
title: "24 days of Hackage, 2015: day 5: should-not-typecheck: making
Haskell sort of dynamically typed with deferred type errors"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 5

Have you ever been frustrated when using a statically typed language
because there's a type error somewhere in your code base but you want
to run your program anyway, either because you don't care about that
remote type error that has nothing to do with what you're working on,
or because you want to step through your code and debug what the type
error really is? I certainly have.

Also, have you ever wanted to write a unit test to verify that your
typed code disallows code you want to disallow, but you are
frustrated because how do you write code in a typed language that
says, "This code (that you won't typecheck) won't typecheck" and passes
the typechecker and runs?

Welcome to the land of GHC's
["deferred type errors"](https://ghc.haskell.org/trac/ghc/wiki/DeferErrorsToRuntime),
a feature that has been part of GHC [since version 7.6.1](https://downloads.haskell.org/~ghc/7.6.1/docs/html/users_guide/defer-type-errors.html) in 2013. Since
this was not covered in Ollie's
[2014 series "24 Days of GHC Extensions"](https://ocharles.org.uk/blog/pages/2014-12-01-24-days-of-ghc-extensions.html),
I decided to bring it up here, and in the context of a cute package, [`should-not-typecheck`](https://hackage.haskell.org/package/should-not-typecheck)
that hooks up with HSpec to make assertions that something won't
typecheck.

<!--more-->

## Installation

Since LTS does not know about this obscure package, Stack helpfully
tells us exactly what to add to our `stack.yaml` to bring it in:

{{< highlight yaml >}}
extra-deps:
- should-not-typecheck-2.0.1
{{< /highlight >}}

## Let's write some tests

The full documentation of `should-not-typecheck` is right there on
[its Hackage page](https://hackage.haskell.org/package/should-not-typecheck).

First, we need to enable the GHC option `-fdefer-type-errors` in the
test module, with a directive:

{{< highlight haskell >}}
{-# OPTIONS_GHC -fdefer-type-errors #-}
{{< /highlight >}}

### Our first test

{{< highlight haskell >}}
import ShouldNotTypecheckExample

import Test.Hspec ( Spec, hspec, describe, it
                  , shouldBe
                  , shouldThrow, anyException
                  )
import Test.ShouldNotTypecheck (shouldNotTypecheck)

spec :: Spec
spec =
  describe "should-not-typecheck" $ do
    it "should not allow mapping negation over a list of strings" $ do
      shouldNotTypecheck (map not ["hello", "world"])
{{< /highlight >}}

That's self-explanatory. We can't do a Boolean negation on a
string. Haskell is not a "truthy"-based language, but a truth-based
language.

## Some puzzling code

Now let's look at the `ShouldNotTypecheckExample` module:

{{< highlight haskell >}}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module ShouldNotTypecheckExample (thisWorks, thisFails) where

thisWorks :: String
thisWorks =
  fst ("hello", ["world" / True, "!"])

thisFails :: String
thisFails =
  snd ("hello", ["world" / True, "!"])
{{< /highlight >}}

Pause for a moment, and think of what should happen when `thisWorks`
and `thisFails` are used, and in what way, and why. In both cases, we
have a tuple and are returning the first element or the second element of
the tuple. The second element is a list that is clearly ill-typed,
because it contains something that is nonsensical (division of a
string by a boolean).

## The role of laziness

To understand what happens in the following tests, you need to
understand how laziness works in Haskell. The word "lazy" has come to
be used for many different ideas and constructs in different
programming languages, but Haskell's "laziness" is unique. A full
discussion is outside the scope of this article, but I thought that
showing what happens with deferred type errors might be a gateway
toward better understanding the execution model of Haskell.

### Never reached

{{< highlight haskell >}}
    it "you can run code even if it contains ill-typed parts" $ do
      thisWorks `shouldBe` "hello"
{{< /highlight >}}

This works because tuples in Haskell are lazy, and therefore in
ordinary typechecking, taking the first element of a well-typed tuple
succeeds no matter what is in the second element of the tuple. The
difference when operating in deferred typechecking mode is that the
tuple doesn't even need to be well-typed, and the second element can
be complete junk, as it is here. So this example is straightforward if
you consider that what GHC does is somehow push the type error into a
reasonably small context so that outside of it, things still typecheck
and run normally.

### Laziness all the way down

So what happens if we get the second element of the tuple, it is junk,
and take its length?

{{< highlight haskell >}}
    it "deferred type errors are only lazily reached" $ do
      length thisFails `shouldBe` 2
{{< /highlight >}}

The answer is that everything is still fine, because the embedded list
inside the lazy tuple is a lazy list (because lists in Haskell are
lazy), and `length` never looks at the elements of the list, only
counts their number, so it passes over the junky thunk for `"world" /
True"` perfectly fine without needing to evaluate it.

### Forcing the laziness

To explicitly force laziness into fully evaluated data (the kind of
data in standard programming languages), we need to use the
[`deepseq`](https://hackage.haskell.org/package/deepseq) package. It's
work to fully, deeply evaluate something in Haskell! We use
[`force`](https://hackage.haskell.org/package/deepseq-1.4.1.2/docs/Control-DeepSeq.html#v:force)
from that package.

In order to catch, in HSpec, the exception we expect to finally get, we
also need to use
[`evaluate`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Exception.html#v:evaluate)
from `Control.Exception` in
[`base`](https://hackage.haskell.org/package/base), the main package
of the ecosystem (discussed
in a [2012 Day of Hackage post](https://ocharles.org.uk/blog/posts/2012-12-23-24-days-of-hackage-base.html)).

{{< highlight haskell >}}
import Control.Exception (evaluate)
import Control.DeepSeq (force)
{{< /highlight >}}

Our test (which for simplicity is coarse in that it catches any
exception, rather than the specific typechecking exception):

{{< highlight haskell >}}
    it "deferred type errors cause an exception only when reached" $ do
      evaluate (force thisFails) `shouldThrow` anyException
{{< /highlight >}}

The deep evaluation will go all the way down to the junky expression
in the list in the tuple of our example, and a typechecking error is
thrown there at run time, as expected.

Suppose we were just evaluating `thisFails` from code, say within
GHCi. This is what we get:

{{< highlight console >}}
*Main> import ShouldNotTypecheckExample
*Main ShouldNotTypecheckExample> thisFails
"*** Exception: /Users/chen/Sync/haskell/twenty-four-days2015-of-hackage/src/ShouldNotTypecheckExample.hs:14:26:
    No instance for (Fractional Char) arising from a use of ‘/’
    In the expression: "world" / True
    In the expression: ["world" / True, "!"]
    In the first argument of ‘snd’, namely
      ‘("hello", ["world" / True, "!"])’
(deferred type error)
{{< /highlight >}}

## Haskell is not really being dynamic here

So is Haskell dynamically typed then, when running in this mode? Not
really. It's faking it. What it's basically doing is that the
typechecker is *still finding the type error at compile time*, but
then secretly creating the exception information at the site of the
crappy code and replacing that code with a call to throw that
exception. The technical details are
[in this paper](http://dreixel.net/research/pdf/epdtecp.pdf).

This is completely different from the dynamic checking where nothing
is checked at compile time and an error is discovered during the
course of run time execution. Here, the error is discovered up front,
stashed away, and kept a secret until or unless it is demanded.

## For more on laziness and forcing

Simon Marlow's free book
["Parallel and Concurrent Programming in Haskell"](http://chimera.labs.oreilly.com/books/1230000000929)
has chapters on evaluation strategies, starting with [chapter 2](http://chimera.labs.oreilly.com/books/1230000000929/ch02.html#sec_par-eval-sudoku2). This stuff is subtle.

## Conclusion

For day 5, I introduced the `should-not-typecheck` package and briefly
discussed Haskell's lazy evaluation and how it interacts with GHC's
deferred type errors. A later Day of Hackage will venture into the
world of doing "real" dynamic typing in Haskell.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
