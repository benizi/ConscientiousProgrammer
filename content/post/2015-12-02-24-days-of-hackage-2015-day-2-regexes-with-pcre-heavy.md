---
categories:
- Haskell
- Hackage
- pcre-heavy
- regexes
- parsers
- parsec
- Perl
- PCRE
- Pittsburgh TechFest
- Template Haskell
comments: true
date: 2015-12-02T07:50:12-05:00
layout: post
title: "24 days of Hackage, 2015: day 2: Regexes with pcre-heavy; standalone Haskell scripts using Stack"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 2

Don't laugh, but once upon a time, I made Perl my main programming
language of choice (between around 1999 and 2010). There were many
reasons for this, but one reason was that Perl made it very easy to do
text processing using regexes.

If you are a seasoned Haskeller, you might be thinking, "Why not use a
real parser instead?", such as the venerable
[parsec](https://hackage.haskell.org/package/parsec), which was covered in a
[2012 day of Hackage](https://ocharles.org.uk/blog/posts/2012-12-10-24-days-of-hackage-parsec.html)?
(Or, today, one could consider one of several other newer alternative libraries
for parsing. A later day of Hackage will say more about this!)

After all, Jamie Zawinski famously once wrote, *"Some people, when
confronted with a problem, think 'I know, I'll use regular
expressions.'  Now they have two problems."* I even gave a talk at
[Pittsburgh Tech Fest](http://pghtechfest.com/) in 2013,
["Stop overusing regular expressions!"](http://www.slideshare.net/FranklinChen/handout-22302440),
in which I promoted writing parsers rather than writing regexes.

But, sometimes I do want to use a regex. In that case, I have been
using an obscure but useful package, [`pcre-heavy`](https://hackage.haskell.org/package/pcre-heavy).

Today I'll show how to use `pcre-heavy`, and while at it, also show
how to ship *one-file standalone Haskell scripts* that only require
Stack.

<!--more-->

## Why use regexes at all?

Before going into `pcre-heavy`, I thought I should explain when I use
regexes.

Back when I was doing a lot of text extraction, cleaning, including
*correction*, restructuring of messy data, regexes seemed the only
choice really. I had to not lose any "intended" information even if it
was obscured by garbage or misspellings or the like. I therefore could
not use some kind of approximate statistical technique, but had to
iteratively do do a lot exploratory work with some interactive
prompting in order to gradually clean up the data. Super-powerful
regex constructs of the Perl variety seemed perfect for this task.

But even outside of such use cases, there's no hiding from the fact
that regexes can be very convenient for simple tasks. Also,
because regexes are used so much in our programming world in general,
if we are migrating to Haskell some already-working regexes from
already-written code in some other language, it's convenient to just
stick with regexes.

## Which Haskell regex library to use?!

A newcomer to Haskell must be overwhelmed by the lack of a single
standard library and syntax for regexes. I mean, take a look at this
[wiki page](https://wiki.haskell.org/Regular_expressions).

Today, I'm presenting
[`pcre-heavy`](https://hackage.haskell.org/package/pcre-heavy), a
regex library that I've been using when I want regexes at all (I try
not to want them). It's pretty new and not even mentioned on that wiki
page.

Some of my criteria for choosing a regex library:

- I want Perl-style regexes. That's what I'm used to and are a kind of
standard across regex support in many programming languages.
- Nice syntax is a plus. One of the selling points of using regexes is
  that the conciseness of writing patterns, binding matches,
  etc. Without such conciseness, I just think "Why not just write a
  real parser? It only takes a couple of lines in Haskell anyway."
- High performance is a perfectly legitimate reason to use regexes.

Given these criteria, using a [PCRE](http://www.pcre.org/)-based
library seemed the way to go. OK, the wiki page lists a bunch of
PCRE-based libraries.

[`pcre-light`](https://hackage.haskell.org/package/pcre-light) is a
good way to go.

**It does require installation of the C library for
PCRE.**

I'm mainly on Mac OS X, so I have PCRE installed through
Homebrew with `$ brew install pcre`. I have PCRE working on
Linux. Unfortunately, I don't use Windows, so if someone can verify
that `pcre-light` installs OK on Windows, that would be great. I would
feel sad if I picked a library that is problematic for Windows users.

Recently, out came
[`pcre-heavy`](https://hackage.haskell.org/package/pcre-heavy), a
wrapper around `pcre-light` that uses
[Template Haskell](https://wiki.haskell.org/Template_Haskell), and I
liked it, so I use it.

## Example program using `pcre-heavy`

`pcre-heavy` has decent documentation on
[its Hackage page](https://hackage.haskell.org/package/pcre-heavy), so
I recommend reading that for the full details on how to use it. I'll
give just a simple example here in the context of a complete program
that does something.

### Specification and some test cases

Say we have a file of lines of text that are supposed to have a
comma-separated format of

- a fixed header
- a text transcript's file path
- an "audio" or "video" field indicating the type of associated media
- an optional annotation about whether the associated media is missing
  or not yet linked into the transcript

(I made up this example based on the structured text specification
called CHAT that happens to include a single line of this format,
e.g. [this coded Supreme Court oral argument transcript for "Citizens United v. Federal Election Commission"](http://talkbank.org/data-orig/Meeting/SCOTUS/2008/08-205.cha).)

Examples that should match:

``` text
@Media:	has-audio,   audio
@Media:	has-video,video
@Media:	has-audio-but-missing, audio, missing
@Media:	has-video-but-unlinked  , video,      unlinked
```

Examples that should fail to match:

``` text
@Media:	no-audio-or-video
@Media:	missing-media-field, unlinked
```

### Creating a regex

Here is a `pcre-heavy` regex, using the
[`re`](https://hackage.haskell.org/package/pcre-heavy-1.0.0.1/docs/Text-Regex-PCRE-Heavy.html#v:re)
Template Haskell
[quasiquoter](https://wiki.haskell.org/Template_Haskell#QuasiQuoters)
that builds a PCRE-compiled
[`Regex`](https://hackage.haskell.org/package/pcre-heavy-1.0.0.1/docs/Text-Regex-PCRE-Heavy.html#t:Regex):


``` haskell
mediaRegex :: Regex
mediaRegex = [re|^@Media:\t([^ ,]+)\ *,\ *(audio|video)(\ *,\ *(?:missing|unlinked))?|]
```

## Regex string validated at Haskell compile-time

One selling point of `pcre-heavy` for me is that because it uses
Template Haskell, a bad regex string results in a Haskell-level
compile-time error rather than a runtime error.

Example of a compile-time error:

``` haskell
-- This Haskell code fails to compile!
mediaRegex :: Regex
mediaRegex = [re|^@Media:\t([^ ,]+)\ *,\ *(audio|video)(\ *,\ *(?:missing|unlinked)?|]
```

Loading this in GHCi or compiling with GHC results in

``` console
    Exception when trying to run compile-time code:
      Text.Regex.PCRE.Light: Error in regex: missing )
    Code: template-haskell-2.10.0.0:Language.Haskell.TH.Quote.quoteExp
            re
            "^@Media:\\t([^ ,]+)\\ *,\\ *(audio|video)(\\ *,\\ *(?:missing|unlinked)?"
```

## Using the regex

We'll use
[`scan`](https://hackage.haskell.org/package/pcre-heavy-1.0.0.1/docs/Text-Regex-PCRE-Heavy.html#v:scan)
to extract the matches (if any) against our regex on a string.

`scan` returns a lazy list of all possible matches:

``` haskell
-- Simplified type signature for our purposes.
scan :: Regex -> String -> [(String, [String])]
```

Each match is a pair `(String, [String])`, where the first component
is the whole string that matched, and the second is an ordered list of
parenthesized groupings in the regex. In our regex, we had three
parenthesized groupings, so a match could result in a three-element
grouping list:

``` console
*Main> scan mediaRegex "@Media:\tfoo, audio, unlinked"
[("@Media:\tfoo, audio, unlinked",["foo","audio",", unlinked"])]
```

Since we only want
the first match (if any), we can just compose it with
[`listToMaybe` from `Data.Maybe`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Maybe.html#v:listToMaybe),
which has type

``` haskell
listToMaybe :: [a] -> Maybe a
```

so `listToMaybe . scan mediaRegex` has type `String -> Maybe (String, [String])`.

``` console
*Main> (listToMaybe . scan mediaRegex) "@Media:\tfoo, audio, unlinked"
Just ("@Media:\tfoo, audio, unlinked",["foo","audio",", unlinked"])
```

## Extracting useful information

Finally, what we really wanted to do after matching is apply
additional business logic and get stuff into a real type as soon as
possible, rather than engage in "stringly-typed" programming and
context-dependent list lengths.

Let's say that for our task, we only care about matched lines that are
*not* missing or unlinked, and skip those that are missing or
unlinked. We define a data type and use pattern matching to get out of
the untyped world into the typed world of our data model.

``` haskell
data Info =
    Skip
  | Audio FilePath
  | Video FilePath
    deriving (Eq, Show)

-- | Extract information about a media file if it is present.
extractIfPresent :: (String, [String]) -> Info
extractIfPresent (_, [name, "audio"]) = Audio name
extractIfPresent (_, [name, "video"]) = Video name
extractIfPresent (_, _) = Skip
```

## Presentation as a report

Finally, now that we are done with the regex world, and have a data
model, all that is left is a driver to complete an example
command-line program.

We have all the information needed to print out a report for each line.

``` haskell
-- | Output a report.
reportOnInfo :: Maybe Info -> IO ()
reportOnInfo Nothing = putStrLn "no match"
reportOnInfo (Just Skip) = putStrLn "match, but missing or unlinked"
reportOnInfo (Just (Audio path)) = printf "audio at %s\n" path
reportOnInfo (Just (Video path)) = printf "video at %s\n" path
```

And the final driver, piping everything through from standard input:

``` haskell
main :: IO ()
main = do
  s <- getContents
  mapM_ (reportOnInfo
        . fmap extractIfPresent
        . listToMaybe
        . scan mediaRegex
       ) (lines s)
```

## Using Stack to ship standalone scripts

We can try our program from within the GHCi REPL by just typing `main`
or `:main` at the REPL prompt and typing in lines of text. We can also
do `stack build` to native-compile into a shippable binary.

But another option is to ship the source code as a standalone one-file
script. This can be very convenient in some circumstances, when you
can rely on the recipient simply installing Stack.

Here's how we can turn our program into such a standalone script: just
add the following two lines and make the file executable:

``` haskell
#!/usr/bin/env stack
-- stack --install-ghc runghc --package pcre-heavy
```

Stack will read the embedded command in order to install GHC, if
 needed, and first download and install the packages listed (here
 `pcre-heavy`), if needed. (Note: in this case, because of FFI with a
 C library, the recipient has to install PCRE first.)

So if you have short programs that don't need to be organized into
full-scale Cabal projects, you can treat Haskell as a "scripting
language" with full access to the libraries of Hackage!

``` console
$ app/PCREHeavyExampleMain.hs < input.txt > output.txt
```

### A warning

Although this Stack-as-Haskell-interpreter feature is kind of cool, I
prefer to write modular, separately testable libraries, while having
the `main` driver of the `Main` module of a program just use library
modules that do most of the real work. Furthermore, I prefer to build
and use native-compiled libraries and binaries because they're just
much faster to start up and also run: `runghc` is a Haskell
interpreter rather than a native optimizing compiler. But the beauty
of the GHC Haskell world is you can run in either mode, and flip from
one to the other seamlessly.

### Here's our complete example standalone program

``` haskell
#!/usr/bin/env stack
-- stack --install-ghc runghc --package pcre-heavy

{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.Regex.PCRE.Heavy
import Data.Maybe (listToMaybe)
import Text.Printf (printf)

-- | Match a media name, audio/video, and optional missing/unlinked.
mediaRegex :: Regex
mediaRegex = [re|^@Media:\t([^ ,]+)\ *,\ *(audio|video)(\ *,\ *(?:missing|unlinked))?|]

data Info =
    Skip
  | Audio FilePath
  | Video FilePath
    deriving (Eq, Show)

-- | Extract information about a media file if it is present.
extractIfPresent :: (String, [String]) -> Info
extractIfPresent (_, [name, "audio"]) = Audio name
extractIfPresent (_, [name, "video"]) = Video name
extractIfPresent (_, _) = Skip

-- | Output a report.
reportOnInfo :: Maybe Info -> IO ()
reportOnInfo Nothing = putStrLn "no match"
reportOnInfo (Just Skip) = putStrLn "match, but missing or unlinked"
reportOnInfo (Just (Audio path)) = printf "audio at %s\n" path
reportOnInfo (Just (Video path)) = printf "video at %s\n" path

-- | Driver, in traditional right-to-left syntax.
main :: IO ()
main = do
  s <- getContents
  mapM_ (reportOnInfo
        . fmap extractIfPresent
        . listToMaybe
        . scan mediaRegex
       ) (lines s)
```

## Some additional notes

One limitation faced by a short expository article with example code
is that we don't like to waste space and attention, and therefore tend
to present quick-and-dirty code, rather than production-level code
(which is efficient, has sensible error recovery, well-commented). I've
been thinking about the dilemma of *how not to give the
wrong impression and set a bad example by showing simplistic example
code*. There's no easy answer, but I felt it might be useful to
provide optional "advanced" notes sometimes, on how to write real code.

`pcre-heavy` allows matching not only of `String`, but also of
`ByteString` and `Text` types. In practice, for efficiency, we
want to use
[`bytestring`](http://hackage.haskell.org/package/bytestring) and
[`text`](http://hackage.haskell.org/package/text) as much as possible,
rather than the inefficient `String` type. ([A 2012 day of hackage
article talks about `text`](https://ocharles.org.uk/blog/posts/2012-12-12-24-days-of-hackage-text.html).)
Since the underlying PCRE C library uses bytes, I generally hand
bytestrings to `pcre-heavy`.

The sample driver code uses lazy I/O to get the lines from input. This
is superficially elegant and concise for pedagogical purposes, but in
real life is a source of resource leaks and other problems and even
causes people to think "Haskell is inefficient". For real work, I like
to use [`pipes`](http://hackage.haskell.org/package/pipes), which was
covered in another
[2012 day of Hackage](https://ocharles.org.uk/blog/posts/2012-12-16-24-days-of-hackage-pipes.html)
and also has an
[extensive, beautiful tutorial](https://hackage.haskell.org/package/pipes-4.1.7/docs/Pipes-Tutorial.html)
by its author, Gabriel Gonzalez, who also has a fantastic,
long-running, active blog
["Haskell for all"](http://www.haskellforall.com/) that every
Haskeller should follow.

Finally, was a regex the right choice here? It was simple enough for
this problem, but you can see from the ad hoc pattern matching and
hardcoded strings and fragile positional ordering and number of groups
that things could get error-prone really quickly if the regex got any
more complex or we wanted to do proper error handling in case of a
failed match.

## Conclusion

Regex support is not a strong point of the Haskell ecosystem, which is
geared to more structured parsing, but there are options if you really
want to use regexes, and I like the Perl-style `pcre-light` family of
libraries that now includes `pcre-heavy`.

Also, I showed how to add two lines to the top of a Haskell program to
turn it into a Stack script.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
