---
categories:
- Haskell
- Hackage
- Hoogle
- Hayoo
- utilities
- MissingH
- extra
- higher-rank types
- PureScript
comments: true
date: 2015-12-06T08:00:00-05:00
layout: post
title: "24 days of Hackage, 2015: day 6: finding utilities with Hoogle
and Hayoo: MissingH, extra"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 6

It will never be the case that everything everyone will find useful
will already be in the "standard library" for any language
ecosystem. However, one of the coolest features of the Haskell
ecosystem (which wows all non-Haskellers when I show them), is the ability to search for useful functions by type
signature, using [`Hoogle`](http://hoogle.haskell.org/) or
[`Hayoo`](http://hayoo.fh-wedel.de/examples). You can use other
criteria also, such as names; this can be useful if you have a guess
at what some useful function might be named.

There seem to be two philosophies as far as using other people's
utility libraries (or even making one's own to share between different
projects):

- reuse is great, let's do it
- every dependency is a potential liability, so it's better to
  reinvent, or copy and paste, rather than use something of uncertain
  quality or maintainability

I tend to prefer reuse, but there have been times when I have copied
(and even modified) only just what I need, because I don't want the
rest of what is inside a sprawling library that depends transitively
on a whole lot of stuff I don't need. I think this is a granularity
issue. Many people have proposed the idea that since we have a Web
now, in theory the concept of "library" should go obsolete in favor of
micro-libraries, so to speak, maybe sometimes even to the level of
single standalone functions, and maybe even having a unique
identifier, but this topic is outside the scope of this article. (For
just one idea, check out Gabriel Gonzalez's
["The internet of code"](http://www.haskellforall.com/2015/05/the-internet-of-code.html).)

The situation is also complicated by the fact that often, so much can
be reinvented with only a couple of lines of Haskell code, so why even
bother looking for someone's implementation of it?

But let's assume for this article that you are interested in finding
and using utility libraries. I show how to find some example functions
and reach two utility libraries that I use, very cleverly and
informatively named
[`MissingH`](http://hackage.haskell.org/package/MissingH) and
[`extra`](http://hackage.haskell.org/package/extra).

<!--more-->

## List/string example

A while ago I was manipulating strings (I was given `String`, as
opposed to `Text` or `ByteString`) and needed to replace all
occurrences of a substring in a file path with a different
substring. For example, as an HSpec test item for a hypothetical
function creatively named `replace`:

{{< highlight haskell >}}
  it "replaces all substrings within a string" $ do
    replace "abc" "d" "123abc123abc" `shouldBe` "123d123d"
{{< /highlight >}}

Sure, it would not be hard to write code to do this, but why not use
see if it's out there already for me to use?

What type should we search for? Maybe

{{< highlight haskell >}}
String -> String -> String -> String
{{< /highlight >}}

i.e.

{{< highlight haskell >}}
String     -- ^ matching substring
-> String  -- ^ replacement for the match
-> String  -- ^ original string
-> String  -- ^ result string
{{< /highlight >}}

OK, let's try this type as a
[Hayoo search](http://hayoo.fh-wedel.de/?query=String+-%3E+String+-%3E+String+-%3E+String). Hmm,
the results are not too promising. At the top is some weird
undocumented regex thing, and that's probably not what we want.

## An important search technique: make the fewest assumptions

Probably the single most important tip for getting good search results
from a type is to make the type as generic as possible: *the more type
variables, the better*, and also use only type class constraints you
need. The operation we want is not really
string-specific. Rather, it was a list operation. So the real type we
want is

{{< highlight haskell >}}
Eq a => [a] -> [a] -> [a] -> [a]
{{< /highlight >}}

This utility function makes the *least* assumptions necessary to get
the job done, while working for `String` because `String` is just
`[Char]` and `Char` is an instance of the `Eq` type class. But for the
purpose of replacements, we don't care about whether we're comparing
characters: we only care that whatever element type is involved in
these subsequences, we can compare for equality.

The
[Hayoo search](http://hayoo.fh-wedel.de/?query=Eq+a+%3D%3E+[a]+-%3E+[a]+-%3E+[a]+-%3E+[a])
immediately brings up much more promising results than with `String`,
from packages such as `utility-ht`, `MissingH`, and `extra`. `pandoc`
also popped up, but that's a
[huge text-processing tool](http://pandoc.org/), not a library I would
pull in for just one tiny utility function!

The
[Hoogle search at `hoogle.haskell.org`](http://hoogle.haskell.org/?hoogle=Eq+a+%3D%3E+[a]+-%3E+[a]+-%3E+[a]+-%3E+[a]&scope=set%3Astackage)
search works pretty well also. (Note that the Hoogle search at the old
site [gives bad results](https://www.haskell.org/hoogle/?hoogle=Eq+a+%3D%3E+[a]+-%3E+[a]+-%3E+[a]+-%3E+[a]).)

## Modifying our tests to check the function with the more generic type

I briefly mentioned refactoring HSpec tests on
[day 3](/blog/2015/12/03/24-days-of-hackage-2015-day-3-hspec-the-importance-of-testing/). Here's
how to test multiple implementations of the same desired function
(let's go with `MissingH` and `extra`), and also test `replace` on
different input types: both `String` (which is just `[Char]`) and `[Int]`:

{{< highlight haskell >}}
module MissingHExtraExampleSpec where

-- | From MissingH
import qualified Data.List.Utils as ListUtils

-- | From extra
import qualified Data.List.Extra as ListExtra

import Test.Hspec (Spec, hspec, describe, it, shouldBe)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "MissingH and extra" $ do
    describeReplace "MissingH" ListUtils.replace
    describeReplace "extra" ListExtra.replace
{{< /highlight >}}

But the following fails to compile! Why?

{{< highlight haskell >}}
-- | Fails to compile!
describeReplace
  :: String  -- ^ description
  -> (Eq a => [a] -> [a] -> [a] -> [a])  -- ^ replace
  -> Spec
describeReplace description replace =
  describe description $ do
    it "replaces all substrings within a string" $ do
      replace "abc" "d" "123abc123abc" `shouldBe` "123d123d"
    it "replaces all int sublists within an int list" $ do
      replace [0 :: Int, 1] [100, 101, 102] [0, 1, 2, 3, 0, 0, 1, 4]
        `shouldBe` [100, 101, 102, 2, 3, 0, 100, 101, 102, 4]
{{< /highlight >}}

### A note on the critical use of higher-rank types for refactoring

The error message is useful if you know what is going on, but not
useful but if not. Yes, we need higher-rank types.

{{< highlight console >}}
    Illegal polymorphic or qualified type:
      Eq a => [a] -> [a] -> [a] -> [a]
    Perhaps you intended to use RankNTypes or Rank2Types
    In the type signature for ‘describeReplace’:
      describeReplace :: String
                         -> (Eq a => [a] -> [a] -> [a] -> [a]) -> Spec
{{< /highlight >}}

Higher-rank types are a
supported GHC extension discussed in a
[2014 Day of GHC Extensions](https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html). Higher-rank
types are tremendously useful and a feature that is missing from type
systems in most other languages.

Briefly, the `replace` function we want has the type

{{< highlight haskell >}}
forall a. Eq a => [a] -> [a] -> [a] -> [a]
{{< /highlight >}}

where we have explicitly quantified the type variable `a` so that the
`Eq` constraint applies inside its scope. Read the type as "for all
types `a` such that `a` is a member of the `Eq` type class, `[a] ->
[a] -> [a] -> [a]`". Ordinary Haskell without the extension doesn't
allow you to write down this type as a parameter into some function,
because it doesn't have explicit `forall` and implicitly inserts a
`forall` for you at the top level for everything, but then that is the
wrong scoping for what we want to say.

So we need to add the directive and change the `replace` parameter
type to have explicit quantification, and all is OK:

{{< highlight haskell >}}
{-# LANGUAGE RankNTypes #-}

describeReplace
  :: String  -- ^ description
  -> (forall a. Eq a => [a] -> [a] -> [a] -> [a])  -- ^ replace
  -> Spec
{{< /highlight >}}

### A note on implicit quantification in Haskell and related languages

I kind of wish type variable quantification were explicit in Haskell,
i.e., *requiring* `forall` annotations, as
[PureScript](http://www.purescript.org/)
[does](https://github.com/purescript/purescript/wiki/Differences-from-Haskell),
because understanding type variable quantification is important for
fully understanding what is going on at the type level in languages
such as ML and Haskell.

For example, here's a good article about
[how to understand the value restriction and monomorphism restriction](http://jozefg.bitbucket.org/posts/2015-03-27-unsafe.html),
which can be puzzling if you don't have the mental model of what is going
on underneath.

## The joy of browsing libraries

One thing that can happen if you find a utility function useful, is
you can browse around in the module that contains it, or the whole
library, just looking for stuff you might find useful in the
future. For example, I find Neil Mitchell's `extra` pleasant enough
(good names and great documentation on Hackage) that I use it when I
can, and I recommend checking it out. The GitHub repo of `MissingH`
suggests that it is not really being updated any more, so I'm
downplaying my use of it.

In the world of physical books and magazines, I still go to my local
libraries and browse both the new book/magazine/DVD shelves as well as
the look around on the shelf of an item I find in the stacks to see if
there's something related that I might enjoy checking out.

## Digging more deeply

Also, note that if you're on the hunt for possibly useful libraries,
but without an immediate need, you can also find them just by looking
at the dependency list that popular libraries already use. I confess
that I have sometimes clicked away on dependencies on a Hackage
page. If you transitively click around on Edward Kmett's
[`lens`](https://hackage.haskell.org/package/lens) dependencies, you
will reach a huge number of useful libraries, because he is the master
of the universe of code reuse.

The physical book or paper analogy here, of course, is looking at the
references or bibliography to find more things to read.

## Conclusion

For day 6, I gave an example of how to search for a function on Hoogle
and Hayoo, and go polymorphic for a good result. I recommend using the
quality `extra` utility library.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
