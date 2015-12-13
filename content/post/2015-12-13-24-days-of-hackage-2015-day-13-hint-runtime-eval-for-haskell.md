---
categories:
- Haskell
- Hackage
- hint
- eval
- dynamic
- GHC
- Lisp
- JavaScript
- Java
comments: true
date: 2015-12-13T08:00:00-05:00
layout: post
title: "24 days of Hackage, 2015: day 13: hint: runtime eval for Haskell"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 13

One hallmark of a "dynamic language" such as Lisp and JavaScript is
the ability to evaluate code at runtime inside a running
process. Since runtime loading of classes is a fundamental feature of
Java, is Java a "dynamic language" then? I think the terms "static
language" and "dynamic language" are not very useful terms, and a
comparison of language and compiler and development environments
should focus on specific features of the user experience and where the
boundaries lie in the semantics. One tricky thing is that a lot of
what is interesting is actually implementation-dependent.

For example, the Haskell standard says nothing about runtime `eval`,
so there is some sense in which Haskell considered as a strictly
defined "language" has no support for it. But if we consider the GHC
implementation and its ecosystem, which is dominant today despite the
existence of other Haskell implementations, there's a lot of tooling
that is "dynamic", in the sense of being able to access GHC APIs in
one of many different ways.

Edward Yang recently wrote an interesting blog post
["The convergence of compilers, build systems and package managers"](http://blog.ezyang.com/2015/12/the-convergence-of-compilers-build-systems-and-package-managers/)
on a subset of the general issue of what can access what for the sake
of tooling. It didn't touch on runtime evaluation, which is an entire
topic in itself.

For today, I decided to mention that you can already do runtime eval
of GHC Haskell code using the package
[`hint`](http://hackage.haskell.org/package/hint) and offer the
thought that maybe we might want something less ad hoc than
third-party packages like this.

<!--more-->

## Why dynamic loading and evaluation of Haskell code?

For me, it always comes down to being jealous of the Lisp world.

There are times when I have wanted to be able to do dynamic loading
and evaluation of Haskell code, and wished I were in Lisp. The main
example is when supporting user-written "plugins" that can be loaded
from a source file or even typed at a custom REPL. The cleanest way of
doing such a thing is to create and implement a limited
domain-specific language and write a parser, type checker (if the DSL
is typed), compiler/interpreter for it. But why do all that if we can
just allow using the full power of Haskell instead?

Luckily, I found libraries such as `hint` that enabled me to do what I
wanted. I'll show a toy example of the kind of thing that I have done.

## The task

Imagine a program that does sorting, and allows the user at runtime to
submit a custom sorting function to have it be used in place of the
default options. For example, the user could have specified the path
of a Haskell source file `OurSorter.hs` as a command-line argument, or
the program could have a preferences dialog box allowing the user to
enter the text of a sorting function.

To make things even more interesting, let's say that the sorting
function to be specified has to be polymorphic, constrained only to
require comparison:

{{< highlight haskell >}}
userDefinedSort :: Ord a => [a] -> [a]
{{< /highlight >}}

How do we load this type of function at runtime?

## A necessary wrapper

The first thing to get out of the way is that we cannot load a
function of type `Ord a => [a] -> [a]` directly. We have to wrap it in
a `newtype` to hide the higher-rank type and go monomorphic:

{{< highlight haskell >}}
{-# LANGUAGE RankNTypes #-}

module SortWrapper (Sort(..)) where

newtype Sort =
  Sort { getSort :: forall a. Ord a => [a] -> [a] }
{{< /highlight >}}

Now we can try to load values of type `Sort` instead of type `Ord a =>
[a] -> [a]`.

## How to load?

Let's create an API called `loadSort` that enables loading a
particular `Sort` by looking for it by module and name. Here's an
HSpec test that illustrates that we want to be able to load a `Sort`
and use it on different types of lists. We're using
`Language.Haskell.Interpreter` to do the work:

{{< highlight haskell >}}
module HintExampleSpec where

import SortWrapper (Sort(Sort))
import HintExample (loadSort)

import qualified Language.Haskell.Interpreter as I

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec =
  describe "hint" $ do
    it "dynamically loads a correct polymorphic sort function" $ do
      Right (Sort ourSort) <-
        I.runInterpreter (loadSort "OurSorter" "ourSort")
      ourSort "ebcad" `shouldBe` "abcde"
      ourSort [1 :: Int, 5, 4, 3, 7] `shouldBe` [1, 3, 4, 5, 7]
    it "dynamically loads a wrong (only head) sort function" $ do
      Right (Sort onlyHead) <-
        I.runInterpreter (loadSort "OurSorter" "onlyHead")
      onlyHead "ebcad" `shouldBe` "e"
      onlyHead [True, False] `shouldBe` [True]

{{< /highlight >}}

## A sample "plugin"

We created a sample "plugin" in a directory whose source code is *not* compiled
into our main program. Imagine that the user has a separate plugins directory.

## The loader

{{< highlight haskell >}}
module HintExample where

import SortWrapper (Sort)
import qualified Language.Haskell.Interpreter as I
import Language.Haskell.Interpreter (OptionVal((:=)))

-- | Dynamically load a 'Sort' implementation from a file.
-- src is needed to pick up our SortWrapper.
-- sort-plugins is a sample user plugins directory
loadSort :: I.MonadInterpreter m =>
            String  -- ^ module name
         -> String  -- ^ function name
         -> m Sort
loadSort moduleName functionName = do
  I.set [I.searchPath := ["src", "sort-plugins"]]
  I.loadModules [moduleName]
  I.setImports [moduleName, "SortWrapper"]
  I.interpret (moduleName ++ "." ++ functionName) (I.as :: Sort)
{{< /highlight >}}

The most interesting part is the use of the `interpret` function that
has type

{{< highlight haskell >}}
interpret :: (MonadInterpreter m, Typeable a) => String -> a -> m a
{{< /highlight >}}

taking a string and a witness for a monomorphic type in order to tell
`interpret` what runtime dictionary for `Typeable` to use.

So there we have it: runtime eval in GHC Haskell. What `hint` provides
is fairly primitive, but I found it useful.

## Conclusion

I'd like to see more official support for dynamism in environments for
languages such as Haskell. This does require access to compiler
internals or official APIs, but I think this is the way to
go. Principled phase separations are important but so is
integration. I like that `hint` exists to allow me to dynamically load
GHC Haskell code.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
