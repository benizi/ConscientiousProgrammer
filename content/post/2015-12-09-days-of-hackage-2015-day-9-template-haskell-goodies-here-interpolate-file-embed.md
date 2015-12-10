---
categories:
- Haskell
- Hackage
- Template Haskell
- here
- interpolate
- file-embed
- strings
comments: true
date: 2015-12-09T08:00:00-05:00
layout: post
title: "24 days of Hackage, 2015: day 9: Template Haskell goodies:
here, interpolate, file-embed"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 9

A stray negative remark I made on
[day 8](/blog/2015/12/08/24-days-of-hackage-2015-day-8-multiset-i-wish-this-were-in-the-standard-containers-package/)
regarding Haskell and its unergonomic support for multi-line string
literals and interpolation led to good comments that I was being
misleading because there actually exist good solutions. I already use
one of them, but had not brought them into the picture because I
didn't want to be distracting in that post by bringing in other
libraries, especially since they are implemented in
[Template Haskell](https://wiki.haskell.org/Template_Haskell), the GHC
extension that is "macros for Haskell", enabling compile-time
metaprogramming (see the
[2014 Day of Hackage article about Template Haskell](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html). On
[day 2](/blog/2015/12/02/24-days-of-hackage-2015-day-2-regexes-with-pcre-heavy-standalone-haskell-scripts-using-stack/)
I already introduced a package that uses Template Haskell, so it looks
like I'll be continuing to do that today.

These packages require only that you turn on `QuasiQuotes` in modules
where you use them, so our example source code will have the header

{{< highlight haskell >}}
{-# LANGUAGE QuasiQuotes #-}
{{< /highlight >}}

<!--more-->

## Better string literals with `here`

I've been using [`here`](http://hackage.haskell.org/package/here) for
"here" strings and interpolation. Check out the
[documentation](https://github.com/tmhedberg/here) for full details,
but for context, here are some examples using data we already have.

Imports:

{{< highlight haskell >}}
import Data.String.Here (hereLit, here, hereFile, i)
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
{{< /highlight >}}

A trimmed multi-line literal, where leading and trailing white space
are removed, making it particularly easy to just copy and paste blocks
of text in between the `here` quasiquoter brackets.

{{< highlight haskell >}}
    it "makes trimmed multi-line strings prettier" $ do
      -- In this case we want trimming.
      let original = "words 3\n\
                     \I 2\n\
                     \have 2\n\
                     \so 2\n\
                     \for 1\n\
                     \like 1\n\
                     \many 1\n\
                     \to 1"
      let trimmedHereDoc = [here|
words 3
I 2
have 2
so 2
for 1
like 1
many 1
to 1
|]
      trimmedHereDoc `shouldBe` original
{{< /highlight >}}

For more control (typically what I do for small examples), use `hereLit`:

{{< highlight haskell >}}
    it "makes literal multi-line strings prettier" $ do
      -- In this case assume we want the trailing newline.
      let original = "words 3\n\
                     \I 2\n\
                     \have 2\n\
                     \so 2\n\
                     \for 1\n\
                     \like 1\n\
                     \many 1\n\
                     \to 1\n"
      let literalHereDoc = [hereLit|words 3
I 2
have 2
so 2
for 1
like 1
many 1
to 1
|]
      literalHereDoc `shouldBe` original
{{< /highlight >}}

But why stop at copy/paste? Much better to embed an actual file with
`hereFile`. We use our own little HSpec discovery `test/Spec.hs` file
as an example:

{{< highlight haskell >}}
    it "allows file embed" $ do
      [hereFile|test/Spec.hs|] `shouldBe`
        "{-# OPTIONS_GHC -F -pgmF hspec-discover #-}"
{{< /highlight >}}

Finally, some interpolation, using `i`. Although it is very
convenient, I have some reservations about overusing this quasiquoter,
because it is engaging in "stringly typed" programming that just makes
use of anything that implements `Show` and `Typeable`. It is easy to
accidentally write code that compiles but does the wrong thing when
operating at this implicit level. Still, it's useful:

{{< highlight haskell >}}
    it "makes interpolation prettier" $ do
      let list = [1 :: Int, 2]
      let num = 42 :: Int
      [i|number: ${num}, stuff: ${map (+1) list}|] `shouldBe`
        "number: 42, stuff: [2,3]"
{{< /highlight >}}

## `interpolate`

[`interpolate`](http://hackage.haskell.org/package/interpolate) is
another package that does the same sort of thing. You might like its
[`unindent`](http://hackage.haskell.org/package/interpolate-0.1.0/docs/Data-String-Interpolate-Util.html)
feature, which facilitates the copy/paste mode of embedding blocks of
text into your code.

## `file-embed`

[`file-embed`](http://hackage.haskell.org/package/file-embed) is also
useful, because you can use it to embed contents of entire
directories. It also has a unique feature of injection into an
executable.

## Conclusion

You don't have to settle for the built-in syntax for creating strings
out of literals in Haskell. With libraries using Template Haskell, you
can use much prettier representations of strings. Check out `here`,
`interpolate`, and `file-embed`.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
