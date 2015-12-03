---
categories:
- Haskell
- Hackage
- HSpec
- RSpec
- testing
- test-driven development
- type-driven development
- Perl
- Ruby
- Elixir
- OCaml
- types
- refactoring
- domain-specific languages
comments: true
date: 2015-12-03T07:55:59-05:00
layout: post
title: "24 days of Hackage, 2015: day 3: HSpec; the importance of testing"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 3

I spent my formative years writing software before "testing framework"
was in my vocabulary, before
["test-driven development"](https://en.wikipedia.org/wiki/Test-driven_development)
was a thing. I shudder to think of those years, because now I'm a
believer in tests and even in test-driven development (TDD), according to my
interpretation of what that means (since everyone has a different
definition).

There are a bunch of testing tools that have been available in the
Haskell ecosystem for some time. In fact, Ollie in his "24 Days of
Hackage" covered

- [`QuickCheck`](https://hackage.haskell.org/package/QuickCheck)
[in 2012](https://ocharles.org.uk/blog/posts/2012-12-08-24-days-of-hackage.html)
- [`doctest`](https://hackage.haskell.org/package/doctest) [in 2013](https://ocharles.org.uk/blog/posts/2013-12-18-doctest.html)
- [`tasty`](http://documentup.com/feuerbach/tasty) [in 2013](https://ocharles.org.uk/blog/posts/2013-12-03-24-days-of-hackage-tasty.html)

and I heartily recommend looking those up.

But today I'm going to show use of [`HSpec`](http://hspec.github.io/)
(noting that a framework like `tasty` or
[`test-framework`](`https://batterseapower.github.io/test-framework/)
are a lot fancier).

<!--more-->

## Why tests?

I first got into writing tests for two reasons:

- Using languages like Perl, it was essentially impossible to
  be productive without writing tests.
- Such languages spawned the tooling to ease the pain of writing,
running, and getting feedback from tests.

But after getting started, I didn't look back, even when using other
languages such as Scala and Haskell. Today, no matter what language
I'm using, I expect there to be a decent testing framework I can
immediately start using. I even did the experiment of
[learning a brand new language, Elixir, through writing tests](http://conscientiousprogrammer.com/blog/2013/08/26/openhack-pittsburgh-learning-elixir-test-driven-and-package-publishing/). I
cannot take a language ecosystem seriously if there is not at least
some reasonable default standard testing framework that is part of it.

There's a myth (or joke) about using languages like Haskell that have
a decent type system: that you don't need tests because you have
types. Hence the unfortunate phrase "tests versus types". I completely
disagree with this. I want my types and I want my tests too: I want to
use every possible tool to help me design, verify, and troubleshoot my
code! At [Pittsburgh TechFest](http://pghtechfest.com/) 2014, I gave a
talk
["Exploring type-directed, test-driven development"](http://www.slideshare.net/FranklinChen/presentation-37257104)
giving my personal view of making the best use of both types and tests
as part of an iterative process of refining understanding and
expression of a solution for a task
(this was before the term "type-directed development" became the title
of a coming book on using Idris,
["Type-directed development with Idris"](https://www.manning.com/books/type-driven-development-with-idris),
whose completion I look forward to!).

The general topic of how best to combine types and tests is well
outside the scope of this article, but I just want to make one claim:
the primary benefits of tests come from their role as *explicit
documentation of intent during a design process*. Ideally, we prefer to
write down expressive types to fully encode intent, and dependently
typed languages such as Idris enable transforming a lot of what used
to be runtime tests into compile-time tests encoded as type checking,
and you can do a bunch of this with Haskell already if you work hard
enough (and
[Dependent Haskell](https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell)
is in progress), but there is nothing wrong with writing tests today
that someday you might turn into types.

### Breaking news!

By sheer coincidence, a
[new testing framework was just announced for OCaml](https://blogs.janestreet.com/testing-with-expectations/).

And today, right after I initially published this article, I found in
my news feed an announcement about [QuickFuzz](http://quickfuzz.org/),
a grammar fuzz tester for Haskell!

It's great that testing is being taken more seriously everywhere and by
everyone.

## Why HSpec?

Why do I use HSpec, and not one of the fancier testing frameworks? I'm
not ruling out migrating to one of those in the future, but for now,
HSpec just feels really easy and comfortable to use, and is good
enough for me. I am so freaking lazy that I might not write tests if I
get intimidated by any possible sources of friction. And I'll admit
that its [Web site](http://hspec.github.io/) is pretty good! Marketing
matters, I guess.

Also, when I was using Ruby, I got accustomed to using
[RSpec](http://rspec.info/), which of course was the inspiration for
HSpec.

## It's all about auto-discovery

Before even saying anything more about HSpec, I want to say that one
selling point of HSpec for me was auto-discovery. Check out the
[manual](http://hspec.github.io/hspec-discover.html) for full details.

Auto-discovery means that given a simple boilerplate setup, you can
use "convention over configuration" and just give test module file
names matching `*Spec.hs` and sticking them anywhere embedded inside
your `test/` directory and they will all be picked up when you run
`stack test`. This means being able to write test modules at will,
rename, delete, add, refactor them and not have to worry about
manually writing a boilerplate driver module that tediously imports
all the test modules and wires them up into a single project test
suite.

Here's the setup I have for all my projects that use HSpec. I provide
it from my sample project template described on
[day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/),
so you can now generate a starter project with HSpec all ready to go
by running

``` console
$ stack new my-new-project franklinchen
```

There is a `test/` directory with a single file in it, the
auto-discovery file named `test/Spec.hs`, which has a single line of
code, actually a comment:

``` haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

This works because when you have HSpec installed, a program
`hspec-discover` also gets installed, and it's called by GHC to do the
work of auto-discovery. Each test module should export `spec`, because
that's what the auto-discovery program will collect to call.

## Writing and refactoring tests

I didn't mention it in
[yesterday's post about using a regex](/blog/2015/12/02/24-days-of-hackage-2015-day-2-regexes-with-pcre-heavy-standalone-haskell-scripts-using-stack/)
to solve a problem, but when I wrote out examples of strings that are
supposed to match a regex and examples of strings that are not
supposed to match it, I simply copied and pasted those examples from
tests I had written.

Let's walk through writing `PCREHeavyExampleSpec.hs`, step by
step.

### Initial version of test code

First, I'll present code that I never actually wrote initially, because I
skipped this step and immediately refactored it in my mind. But I
decided that to showcase Haskell's strength as a language for
embedding a domain-specific language (DSL), I retroactively wrote the
most obvious code that shows how HSpec works without introducing
non-HSpec considerations. (The code is on branch [`boilerplated-hspec`](https://github.com/FranklinChen/twenty-four-days2015-of-hackage/tree/boilerplated-hspec).)

``` haskell
module PCREHeavyExampleSpec where

import PCREHeavyExample (mediaRegex)

import Test.Hspec (Spec, describe, it, shouldSatisfy)
import Text.Regex.PCRE.Heavy ((=~))

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "pcre-heavy" $ do
    describe "match" $ do
      it "has audio" $ do
        "@Media:\thas-audio,   audio" `shouldSatisfy` (=~ mediaRegex)
      it "has video" $ do
        "@Media:\thas-video,video" `shouldSatisfy` (=~ mediaRegex)
      it "has audio but missing" $ do
        "@Media:\thas-audio-but-missing, audio, missing" `shouldSatisfy` (=~ mediaRegex)
      it "has video but unlinked" $ do
        "@Media:\thas-video-but-unlinked  , video,      unlinked" `shouldSatisfy` (=~ mediaRegex)
    describe "no match" $ do
      it "no audio or video" $ do
        "@Media:\tno-audio-or-video" `shouldSatisfy` (not . (=~ mediaRegex))
      it "missing media field" $ do
        "@Media:\tmissing-media-field, unlinked" `shouldSatisfy` (not . (=~ mediaRegex))
```

The main thing to understand is that for simplest use (without
fixtures, effects, etc.), a basic description-labeled spec item is
introduced with `it`, and a labeled `describe` can contain many of
those as well as sub-`describe`s.

Here, we have two sub-`Spec`s, one for examples that *should match* the regex
and one for examples that *should not*.

Note that we imported and used `mediaRegex` from module
`PCREHeavyExample`.

Unlike in our example program yesterday, which used `scan` from
`pcre-heavy` to collect match bindings, we only care whether something
matched, so we use its `=~` operator instead that takes an input
string and a regex, and returns a `Bool`.

The test code is concise enough, and the problem domain well
understood enough, that even if the syntax looks strange, I hope it is
clear *what* is going on, even if not clear *how* it's being done.

### A note on syntax in Haskell code

Now is a good time to talk about the issue of syntax in Haskell code,
because I'm expecting that if you are reading this, you might not
already be familiar with HSpec, and I also cannot assume that you are
already a seasoned Haskell developer, because I'm writing this article
series not for advanced Haskellers but for those starting to dip into
the library ecosystem and even friends with limited experience with
Haskell.

It is convenient to use
[operator sectioning syntax](https://wiki.haskell.org/Section_of_an_infix_operator)
above, but I could have written

``` haskell
text `shouldSatisfy` (\inputString -> inputString =~ mediaRegex)
```

Furthermore, it is also convenient to use
[infix syntax for named functions](https://wiki.haskell.org/Infix_operator)
when sensible, but it is not required. I could have written in
bare-bones style

``` haskell
shouldSatisfy text (\inputString -> inputString =~ mediaRegex)
```

And the cute `(not . (=~ mediaRegex))` can be written as

``` haskell
\inputString -> not (inputString =~ mediaRegex)
```

I mention these facts about syntax because I have often been told by
people looking into Haskell that it's confusing because of all the
operator syntax. But you don't have to use this syntax if you don't
want to: much that looks weird in Haskell is not something about the
language itself, but just about optional syntax for which there is
"normal" syntax if you prefer that. It's not just about operators, but
about a lot of other optional syntax as well; if you are still
relatively new to Haskell syntax, Gabriel Gonzalez wrote a nice
"syntax decoding" tutorial covering some of that
[here](http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html).

But saying "you don't have to write it" is no use if "everyone is doing
it" and you have to *read* it anyway. So it is the community as a whole
that sets the tone for what gets written and what gets read.

One reason I like HSpec is that it does not go overboard with
syntax. [HUnit](https://hackage.haskell.org/package/HUnit), an older
testing framework, provided funny operators that really turned me off,
such as
[`~=?`](https://hackage.haskell.org/package/HUnit-1.3.0.0/docs/Test-HUnit-Base.html). I
like Gabriel Gonzalez's article
["How to make your Haskell code more readable to non-Haskell programmers"](http://www.haskellforall.com/2015/09/how-to-make-your-haskell-code-more.html). It
applies also to making the code more readable to experienced Haskell
programmers!

I admit to having been guilty of some practices he calls out. I have
mixed feelings about giving them all up, all the time. For example, it
seems idiomatic to use the infix function operator `$` for embedded
DSLs such HSpec, rather than parenthesize everything. I'm curious what
you think. Would you prefer to read the following, which is what the
`$` operator avoids requiring?


``` haskell
spec :: Spec
spec =
  describe "pcre-heavy" (do
    describe "match" (do
      it "has audio" (do
        "@Media:\thas-audio,   audio" `shouldSatisfy` (=~ mediaRegex)
        )
      -- ...
      )
    describe "no match" (do
      -- ...
      )
    )
```

I personally think that languages with a `begin`/`end` kind of block
(such as Pascal, Ruby) instead of braces or parentheses have an
advantage because that is more readable (to me), and recent research
["An empirical investigation into programming language syntax"](http://dl.acm.org/citation.cfm?id=2534973)
claims to have evidence of this.

Meanwhile, we make do with the language we have, and learn and teach
its quirks and features. It's regrettable that English and Chinese are
really hard languages to use too, but we make do if we want to be part
of the community in the United States or in China. It goes both ways:
if we want to be part of the community, we have to invest in
understanding how it operates, and if the community wants to grow, it
has to reach out to newcomers rather than just say "you're on your
own, deal with it". Think of the immense amount of effort that goes
into promoting universal literacy.

### A one-minute review of test-driven development

Let's continue with the test writing process.

When doing test-driven development, we write an HSpec spec first,
*before even writing any implementation code*. Test-driven development is
where you show how something is supposed to work before you actually
write that something. In a typed setting, this means we get a
compile-time error when first trying to run the test, which we fix by
creating `PCREHeavyExample` as a new module with a stub:

``` haskell
module PCREHeavyExample (mediaRegex) where

mediaRegex = undefined
```

Of course, every test fails (in the terminal, the failures are
highlighted in red):

``` console
$ stack test
PCREHeavyExample
  pcre-heavy
    match
      has audio FAILED [1]
      has video FAILED [2]
      has audio but missing FAILED [3]
      has video but unlinked FAILED [4]
    no match
      no audio or video FAILED [5]
      missing media field FAILED [6]

Failures:

  test/PCREHeavyExampleSpec.hs:13:
  1) PCREHeavyExample.pcre-heavy.match has audio
       uncaught exception: ErrorCall (Prelude.undefined)

  test/PCREHeavyExampleSpec.hs:15:
  2) PCREHeavyExample.pcre-heavy.match has video
       uncaught exception: ErrorCall (Prelude.undefined)

  test/PCREHeavyExampleSpec.hs:17:
  3) PCREHeavyExample.pcre-heavy.match has audio but missing
       uncaught exception: ErrorCall (Prelude.undefined)

  test/PCREHeavyExampleSpec.hs:19:
  4) PCREHeavyExample.pcre-heavy.match has video but unlinked
       uncaught exception: ErrorCall (Prelude.undefined)

  test/PCREHeavyExampleSpec.hs:22:
  5) PCREHeavyExample.pcre-heavy, no match, no audio or video
       uncaught exception: ErrorCall (Prelude.undefined)

  test/PCREHeavyExampleSpec.hs:24:
  6) PCREHeavyExample.pcre-heavy, no match, missing media field
       uncaught exception: ErrorCall (Prelude.undefined)
```

#### A tangent on GHC's error reporting

A super-annoying thing, and a long-standing embarrassment for GHC, is
that using `undefined` doesn't trigger useful error reporting. I
look forward to
[GHC 8.0](https://ghc.haskell.org/trac/ghc/wiki/Status/GHC-8.0.1)'s
new feature of
[implicit parameters providing callstacks/source locations](https://ghc.haskell.org/trac/ghc/wiki/ExplicitCallStack/ImplicitLocations). This
stuff is important! It's time we got line numbers and call stacks for
errors without having to jump through hoops.

### Skipping to the end, assume we finished the implementation

OK, let's assume we finished the implementation, which is simply
writing the regex for `mediaRegex`. Then the tests pass (and in the
terminal they display in green):

``` console
PCREHeavyExample
  pcre-heavy
    match
      has audio
      has video
      has audio but missing
      has video but unlinked
    no match
      no audio or video
      missing media field

Finished in 0.0010 seconds
6 examples, 0 failures
```

## Tests are code too!

It's easy to not take test code seriously and not hold it up to the
same standards as "regular" code. That is a mistake: test code should
actually be cleaner and tighter than main implementation code because
it is our *executable documentation* and what we need to make as easy
to read, write, and modify as requirements change.

### Refactoring, part 1

Notice the tremendous amount of code duplication in the tests. We can
do better than this item-by-item copy-and-paste job. We can write code
to generate all the matching examples, by refactoring the relevant
data into a table and a function that maps over the table to get a
composite `Spec`.

Here is a table that pairs a test description with each example input
string:

``` haskell
matchExamples :: [(String, String)]
matchExamples =
  [ ( "has audio"
    , "@Media:\thas-audio,   audio"
    )
  , ( "has video"
    , "@Media:\thas-video,video"
    )
  , ( "has audio but missing"
    , "@Media:\thas-audio-but-missing, audio, missing"
    )
  , ( "has video but unlinked"
    , "@Media:\thas-video-but-unlinked  , video,      unlinked"
    )
  ]

```

Here is a function that generates a spec item given a description/input pair.

``` haskell
matchSpec :: (String, String) -> Spec
matchSpec (description, text) =
  it description $ do
    text `shouldSatisfy` (=~ mediaRegex)
```

Similarly for the non-matching examples.

And the refactored `Spec`:

``` haskell
spec :: Spec
spec =
  describe "pcre-heavy" $ do
    describe "match" $ do
      mapM_ matchSpec matchExamples
    describe "no match" $ do
      mapM_ nonMatchSpec nonMatchExamples
```

### Refactoring, part 2

Uh oh, I said "similarly". Usually when something is "similar",
there's more refactoring that might be doable.

But **Haskell makes refactoring joyful**.

Haskell is a expressive language, where "might" usually means "can",
and "can" often means "should". In my experience, Haskell's *single
best quality* in terms of user experience is its support for
refactoring at will and with confidence that everything will still
mean exactly the same thing after as before the refactoring.

I particularly look forward to the
ongoing development of a
[universal Haskell IDE engine](https://github.com/haskell/haskell-ide-engine)
refactoring even easier, e.g., folding in
[`HaRe`](http://www.cs.kent.ac.uk/projects/refactor-fp/) support.

We see a pattern of positive examples and negative examples using
a predicate and its negation. Let's abstract this pattern out. Let's
collect the positive and negative examples in one place. For
simplicity, let's tuple them.

And now that we're dealing with arbitrary predicates, we no longer
have to hardcode `(=~ mediaRegex)` or `String` everywhere. We can *go
polymorphic* in the predicate type, replacing `matchSpec` and
`nonMatchSpec` with a single `predSpec`.

The final result:

``` haskell
spec :: Spec
spec =
  describePredicate "pcre-heavy"
    ("match", (=~ mediaRegex))
    (matchExamples, nonMatchExamples)

describePredicate :: Show a =>
     String                           -- ^ description
  -> (String, a -> Bool)              -- ^ (base description, predicate)
  -> ( [(String, a)], [(String, a)] ) -- ^ positive and negative examples
  -> Spec
describePredicate description
                  (baseDescription, predicate)
                  (positiveExamples, negativeExamples) =
  describe description $ do
    describe baseDescription $ do
      mapM_ (predSpec predicate) positiveExamples
    describe ("not " ++ baseDescription) $ do
      mapM_ (predSpec (not . predicate)) negativeExamples

predSpec :: Show a => (a -> Bool) -> (String, a) -> Spec
predSpec predicate (description, a) =
  it description $ do
    a `shouldSatisfy` predicate
```

Note that `describePredicate` and `predSpec` can then be pulled out
into a test utilities module for use by other specs using the same
pattern.

Unfortunately, this refactoring, although good in some ways, came with
a cost. It doesn't look so great to me. Does it to you?

### Refactoring, part 3?

One reason the refactored code doesn't actually look so great now is
that our refactoring led to many nested primitive types
(["primitive obsession"](http://c2.com/cgi/wiki?PrimitiveObsession))
and an explosion in number of positional arguments to our new
`describePredicate`. Let's face it, calling `describePredicate` is
cryptic, calling out for "keyword arguments" (in a language that
supports them).

In Haskell, "keyword arguments" means there's a configuration data
type crying to be defined. A related code smell is that documenting
the parameters to `describePredicate` is now super-awkward. Each of
those parameters should be a thing in itself, not just parenthesized,
bracketed, tupled glop.

If we are really serious about refactoring, we should wrap these
things into new data types that are an explicit model of what we want
to do when classifying and testing examples.  We might even turn the
whole thing into its own embedded sub-DSL of HSpec.

This illustrates how refactoring can sometimes lead to new complexity
that didn't exist before. There are tradeoffs constantly. Abstraction
for its own sake does not always make things clearer. For this reason,
I did not actually go this far initially for the example code
yesterday: I did not feel it was worth the trouble. I've left it in
the [`refactoring-2`](https://github.com/FranklinChen/twenty-four-days2015-of-hackage/tree/refactoring-2) branch of the GitHub repo.

## Combining testing frameworks

One last thing about HSpec: you can use it within a larger testing
framework, or you can embed another testing framework into it as
well. For example, I like to use
[QuickCheck through HSpec](http://hspec.github.io/quickcheck.html) as
part of "type-directed development".

## Conclusion

Testing is important, but few love to do it. Making it easy to write
and use tests goes a long way toward actually doing it. I like HSpec
because it's easy to write, and because of auto-discovery. I hope you
consider using it for your own projects if you don't already use it or
some other testing framework.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
