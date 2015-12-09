---
categories:
- Haskell
- Hackage
- multiset
- containers
- text
- sorting
- builder pattern
comments: true
date: 2015-12-08T08:00:00-05:00
layout: post
title: "24 days of Hackage, 2015: day 8: multiset; I wish this were in
the standard containers package"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 8

I don't remember when it was, but one day I got sick and tired of
reimplementing in Haskell the same boilerplate logic when keeping
track of many objects classified under the same key. This is a data
structure known as a
[multiset](https://en.wikipedia.org/wiki/Multiset), also known as a
*bag*, in contrast with a regular *set*, which only keeps track of one
unique object per key.

I was surprised that by the lack of a multiset module in the standard
[`containers`](https://hackage.haskell.org/package/containers)
package, although not completely surprised because you can implement a
multiset on top of a set, so in some sense a multiset is
"superfluous". Still, I was used to having a multiset available
without any work (however trivial), because of long using a [multiset
in C++](http://www.cplusplus.com/reference/set/multiset/), which I'd
used back in the 1990s from the original implementation of the
[Standard Template Library](https://en.wikipedia.org/wiki/Standard_Template_Library),
and also Python's collections library has a
[`Counter`](https://docs.python.org/2/library/collections.html#collections.Counter)
class which serves a similar purpose.

Today I'll briefly show some code using `multiset` and talk about why
something like this maybe should be in the standard library.

<!--more-->

## The classic example of multiset use

Multisets are often used for counting things. Word count is a
time-honored use case for multisets, so let's implement a very simple
word count program that break up text into "words" separated by
spaces, while treating non-letters as spaces, count the words, and
finally output a report which consists of a line for each word and its
count, in descending order of count but ascending order of word.

For a bit more realism and efficiency even for a toy program, let's
have the input not be `String`, but the `Text` type from
`Data.Text.Lazy` module of the
[`text`](https://hackage.haskell.org/package/text) package. Let's also
build up the final `Text` report efficiently.

### A sample HSpec test

Here's a sample test (for fun you may wish to write a reasonable
QuickCheck generators and tests for this problem). I apologize for the
illegible Haskell multiline string literal; I wish Haskell had
multiline string literals, interpolation, and all that good stuff
other languages have!

#### (A tangent from day 9)

A number of people commented that I made a misleading remark above
about string support in Haskell. It is true that the core Haskell
syntax for strings is limited, but it is easy to work around that with
Template Haskell, so see
[Day 9](/blog/2015/12/09/24-days-of-hackage-2015-day-9-template-haskell-goodies-here-interpolate-file-embed/)
for how to do this. I totally recommend using one of the libraries
mentioned.

In brief, the ugly string literal I wrote can be written

{{< highlight haskell >}}
      let expected = [hereLit|words 3
I 2
have 2
so 2
for 1
like 1
many 1
to 1
|]
{{< /highlight >}}

#### The code

{{< highlight haskell >}}
{-# LANGUAGE OverloadedStrings #-}

module MultisetExampleSpec where

import MultisetExample (wordCount)

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "multiset" $ do
    it "wordCount" $ do
      let input = "I have so   many words; words I so like to have words for!?"
      let expected = "words 3\n\
                     \I 2\n\
                     \have 2\n\
                     \so 2\n\
                     \for 1\n\
                     \like 1\n\
                     \many 1\n\
                     \to 1\n"
      wordCount input `shouldBe` expected
{{< /highlight >}}

### The solution

The code almost writes itself from a natural language description of
the problem.

{{< highlight haskell >}}
{-# LANGUAGE OverloadedStrings #-}

module MultisetExample where

import qualified Data.MultiSet as MultiSet
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Builder as LazyBuilder
import Data.Text.Lazy.Builder.Int (decimal)
import Data.Ord (Down(..))
import qualified Data.Char as Char
import qualified Data.List as List
import Control.Arrow ((>>>))
import Data.Monoid ((<>))

wordCount :: LazyText.Text -> LazyText.Text
wordCount =
  LazyText.map replaceNonLetterWithSpace
  >>> LazyText.words
  >>> MultiSet.fromList
  >>> MultiSet.toOccurList
  >>> List.sortOn (snd >>> Down)
  >>> map summarizeWordCount
  >>> mconcat
  >>> LazyBuilder.toLazyText

replaceNonLetterWithSpace :: Char -> Char
replaceNonLetterWithSpace c
  | Char.isLetter c = c
  | otherwise = ' '

summarizeWordCount :: (LazyText.Text, MultiSet.Occur) -> LazyBuilder.Builder
summarizeWordCount (word, count) =
  LazyBuilder.fromLazyText word <> " " <> decimal count <> "\n"
{{< /highlight >}}

Let's go through this step by step. Note that we're using my favorite
left-to-right composition operator `>>>` as opposed to the
right-to-left composition operator `.` because it looks much more
natural to me for pipelines, and because my description below top to
bottom matches this syntax.

#### (Update on syntax)

Here is the pipeline part of the code in more traditional
right-to-left syntax. I think it's important to be able to read and
write in both styles:

{{< highlight haskell >}}
-- | Same thing, using traditional right-to-left composition.
wordCountTraditional :: LazyText.Text -> LazyText.Text
wordCountTraditional =
  LazyBuilder.toLazyText
  . mconcat
  . map summarizeWordCount
  . List.sortOn (Down . snd)
  . MultiSet.toOccurList
  . MultiSet.fromList
  . LazyText.words
  . LazyText.map replaceNonLetterWithSpace
{{< /highlight >}}

### Get the words from text

First, replace all non-letters with spaces for the purpose of this
problem, then use the `words` from `Data.Text.Lazy` (which we imported
as `LazyText`) to break up into words (real tokenization would have
more sophisticated rules). By the way, this is done very efficiently
because GHC performs
[fusion](https://hackage.haskell.org/package/text-1.2.1.3/docs/Data-Text-Lazy.html#g:1)
in order to do the replacement and the breaking up into words in a
single pass, rather than two as written in the code. Fusion is a
really, really cool optimization that we rely on for our Haskell code
to be super-efficient:

{{< highlight haskell >}}
wordCount =
  LazyText.map replaceNonLetterWithSpace
  >>> LazyText.words
{{< /highlight >}}

### Put the words into a multiset

Next, we get the list words into a multiset. `MultiSet` conveniently
has a constructor for that, [`fromList :: Ord a => [a] -> MultiSet a`](https://hackage.haskell.org/package/multiset-0.3.0/docs/Data-MultiSet.html#v:fromList),
and it does this in `O(n log n)` time because of the use of comparison
through `Ord` to construct a tree (exercise for you if you wish: use
one of the hash table libraries in Hackage to create a
`MultiHashSet` instead).

{{< highlight haskell >}}
  >>> MultiSet.fromList
{{< /highlight >}}

#### Count the words

We grab a list of word/count pairs (in ascending order of words) using
[`toOccurList :: MultiSet a -> [(a, Occur)]`](https://hackage.haskell.org/package/multiset-0.3.0/docs/Data-MultiSet.html#v:toOccurList)
where
[`type Occur = Int`](https://hackage.haskell.org/package/multiset-0.3.0/docs/Data-MultiSet.html#t:Occur):

{{< highlight haskell >}}
  >>> MultiSet.toOccurList
{{< /highlight >}}

#### Sort the word/count list

We sort using the
["Schwartzian transform"](https://en.wikipedia.org/wiki/Schwartzian_transform)
for efficiency using
[`Data.List.sortOn`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html#v:sortOn)
of type `sortOn :: Ord b => (a -> b) -> [a] -> [a]`;
recall we want the count (the second element of the tuple we get back
from the multiset) to be descending.

Documentation of
[`Data.Ord.Down`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Ord.html#t:Down)
explains how this "newtype hack" is used to change the default
ordering comparison during a sort.

{{< highlight haskell >}}
  >>> List.sortOn (snd >>> Down)
{{< /highlight >}}

#### Make a report for each word/count

{{< highlight haskell >}}
  >>> map summarizeWordCount
  >>> mconcat
{{< /highlight >}}

where our summarizing function, for efficiency in avoiding unnecessary
allocations, does not construct a `Text` but rather a
[builder](https://en.wikipedia.org/wiki/Builder_pattern), since we
don't need this information for each as `Text` but plan to combine the
reports for all the lines into one `Text` at the end.

{{< highlight haskell >}}
summarizeWordCount :: (LazyText.Text, MultiSet.Occur) -> LazyBuilder.Builder
summarizeWordCount (word, count) =
  LazyBuilder.fromLazyText word <> " " <> decimal count <> "\n"
{{< /highlight >}}

(If you're curious how the builder works, check out the
[source code](https://hackage.haskell.org/package/text-1.2.1.3/docs/src/Data-Text-Internal-Builder.html). It
uses higher-rank types, unsafe operations, and strictness
annotations.)

#### Make the final report

Just materialize the `Text` from the builder:

{{< highlight haskell >}}
  >>> LazyBuilder.toLazyText
{{< /highlight >}}

## What should go into a standard library?

I mentioned that I think this should go into the standard library, for
convenience. The argument for *not* putting something into the
standard library is always, everyone think their useful thing should
be in there, but there's no way that can happen, and so choices have
to be made. Given that, I cannot really argue based on my own
experience or perception of my experience that an entire community
would benefit from my favorite things being made "standard". I'd like
to think that we have the technology now to collect real data to
determine people's real needs and change the way we make libraries
discoverable and usable. Even something like mining Stack Overflow
questions could bring up many interesting statistics about what people
need. Such data gathering would not substitute for expert curation, of
course, but surely would be useful.

There is another argument for not having something like multiset be
incorporated: it's easy to implement once you have the regular set. If
you look at the
[source code](https://hackage.haskell.org/package/multiset-0.3.0/docs/src/Data-MultiSet.html#MultiSet),
the following basically says it all. A multiset is just a map from a
key to a count.

{{< highlight haskell >}}
newtype MultiSet a = MS { unMS :: Map a Occur }

type Occur = Int
{{< /highlight >}}

Everything else is a straightforward wrapper.

But that's like saying a language should not have any syntactic
sugar. I think "library sugar" is as valuable as syntactic
sugar.

## Conclusion

Multisets are useful. You could implement one yourself. Or you could
use the `multiset` package someone already wrote for us.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
