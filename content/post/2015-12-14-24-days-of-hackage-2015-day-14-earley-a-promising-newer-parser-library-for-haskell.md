---
categories:
- Haskell
- Hackage
- Earley
- parsec
- parsers
- ambiguity
- nonempty lists
comments: true
date: 2015-12-14T08:00:00-05:00
layout: post
title: "24 days of Hackage, 2015: day 14: Earley: a promising newer
parser library for Haskell"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 14

On
[day 10](/blog/2015/12/10/24-days-of-hackage-2015-day-10-s-cargot-using-s-expression-syntax/),
I showed how to use S-expressions to avoid having to write a custom
parser. But writing parsers isn't too bad in Haskell, or is it? The
popular `parsec` library
[has many problems](http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/),
because it requires hand-hacked backtracking that causes weird error
messages and difficulty in reasoning about your grammar. There's an
improved fork of `parsec` called
[`megaparsec`](https://hackage.haskell.org/package/megaparsec), but
it's still the same kind of technology. How about something completely
different.

The recent [`Earley`](https://hackage.haskell.org/package/Earley) is
intriguing and I've begun using it for new projects where I don't need
the monadic power of something like `parsec` but are OK with an
applicative API instead and don't need the performance of something
like `attoparsec`. Apart from good error messages, it allows handles
online parsing and ambiguity.

Today I'll give two small examples of using `Earley`.

<!--more-->

## Installation

Since Stackage LTS is behind right now, and `Earley` keeps moving,
I decided to use the latest version of `Earley` by modifying our
`stack.yaml`:

{{< highlight yaml >}}
- Earley-0.10.1.0
{{< /highlight >}}

## Parsing into day 10's AST

Let's go back to the symbolic differentiation problem on day 10, and
create a math-like infix syntax to parse.

### Tests

Here are some HSpec/QuickCheck tests to illustrate what we want when
parsing a string into an `Exp`.

#### Imports

`Text.Earley` is the main module of the `Earley` package; `Report` is
used for return a report on the progress of the parse.

{{< highlight haskell >}}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module SymbolicDifferentiation.EarleySpec where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))
import qualified SymbolicDifferentiation.Earley as Earley
import Text.Earley (Report(..))

import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative(..))

import Data.String.Here (i)
{{< /highlight >}}

#### QuickCheck tests

Some QuickCheck tests that show that some sample expressions such as
`x*a + y*b * (z+c)` parse into the expected ASTs:

{{< highlight haskell >}}
spec :: Spec
spec =
  describe "Custom syntax for expression parsed by Earley" $ do
    -- For simplicity, don't support negative numeric literals now.
    prop "x + a" $ \(NonNegative (a :: Int)) ->
      Earley.parses [i|x + ${a}|] `shouldSatisfy`
        \case
          ([Plus (V "x") (N a)], _) -> True
          _ -> False

    prop "x*a + y*b * (z+c)" $
      \(NonNegative (a :: Int))
       (NonNegative (b :: Int))
       (NonNegative (c :: Int)) ->
      Earley.parses [i|x*${a} + y*${b} * (z+${c})|] `shouldSatisfy`
        \case
          ([Plus (Times (V "x") (N a))
                 (Times (Times (V "y") (N b))
                        (Plus (V "z") (N c)))], _) -> True
          _ -> False
{{< /highlight >}}

#### Expected parse errors

Finally, one example of how to check for expected parse errors. The
error tokens are user-defined and attached to grammar productions, as
we will see.

{{< highlight haskell >}}
    it "x + y * + 5" $
      Earley.parses "x + y * + 5" `shouldSatisfy`
        \case
          ([], Report { position = 8
                      , expected = ["number", "identifier", "("]
                      , unconsumed = "+ 5"
                      }) -> True
          _ -> False
{{< /highlight >}}

### Implementation

The implementation involves `Applicative` idioms that will be familiar
to you if you have used `parsec`.

#### Imports

{{< highlight haskell >}}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}

module SymbolicDifferentiation.Earley where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))

import qualified Text.Earley as E
import Text.Earley ((<?>))
import Control.Applicative (many, some, (<|>))
import qualified Data.Char as Char
import Control.Monad.ST (ST)
import Data.ListLike (ListLike)

-- | What to report for something expected.
type Expected = String
{{< /highlight >}}

The `<?>` operator is used to attach an expectation (which we have
decided to specify as a string, with type synonym `Expected`) to a
production.

#### Drivers

What we want for our particular problem is a parser that takes a
string as input and expects to fully parse it. We construct it from a
more generic parser that comes from processing our grammar.

{{< highlight haskell >}}
-- | Return a list of all possible `Exp` parses, and also a status report
-- regardless of how many successes.
parses :: String -> ([Exp], E.Report Expected String)
parses = E.fullParses expParser

-- | Parser created from the grammar.
expParser :: ListLike input Char =>
             ST state (input -> ST state (E.Result state Expected input Exp))
expParser = E.parser grammar
{{< /highlight >}}

#### Grammar

Our grammar is straightforward. `Earley` uses a monad to maintain its
internal state, and we use the `RecursiveDo` GHC extension (covered in
a
[2014 Day of GHC Extensions](https://ocharles.org.uk/blog/posts/2014-12-09-recursive-do.html))
in order to be able to refer to a rule within the grammar
recursively. Note that left recursion in the grammar is just fine for
`Earley`.

`Prod` is the [type constructor for a production](https://hackage.haskell.org/package/Earley-0.10.1.0/docs/Text-Earley.html#t:Prod), and you build up
productions using combinators such as `satisfy` and `symbol`.

{{< highlight haskell >}}
-- | Basically taken from <https://github.com/ollef/Earley/blob/master/examples/Expr2.hs Earley example expression parser>
grammar :: forall r. E.Grammar r (E.Prod r Expected Char Exp)
grammar = mdo
  whitespace <- E.rule $
    many $ E.satisfy Char.isSpace

  let token :: E.Prod r Expected Char a -> E.Prod r Expected Char a
      token p = whitespace *> p

      sym x   = token $ E.symbol x <?> [x]

      ident   = token $ (:) <$> E.satisfy Char.isAlpha
                            <*> many (E.satisfy Char.isAlphaNum)
                            <?> "identifier"
      num     = token $ some (E.satisfy Char.isDigit) <?> "number"
      -- For now, just handle unsigned numeric literals.

  atom <- E.rule $
    (N . read) <$> num
    <|> V <$> ident
    <|> sym '(' *> term <* sym ')'

  factor <- E.rule $
    Times <$> factor <* sym '*' <*> atom
    <|> atom

  term <- E.rule $
    Plus <$> term <* sym '+' <*> factor
    <|> factor

  return $ term <* whitespace
{{< /highlight >}}

For more examples of grammars, see the
[examples directory in the `Earley` GitHub repo](https://github.com/ollef/Earley/tree/master/examples).

## For fun: solving the "number word" problem

The ability to handle ambiguity and return all possible parses is a
useful one in many situations. Here I show a solution to the
["number word"](http://programmingpraxis.com/2014/07/25/number-words/)
problem. In the past, I have managed ambiguity using
[Happy's GLR support](https://www.haskell.org/happy/doc/html/sec-glr.html),
but I don't like writing parsers using Happy.

The "number word" problem:

{{< highlight text >}}
Given a positive integer, return all the ways that the integer can be
represented by letters using the mapping 1 -> A, 2 -> B, ..., 26 ->
Z. For instance, the number 1234 can be represented by the words ABCD,
AWD and LCD.
{{< /highlight >}}

This is a toy version of an actually serious problem, that of
[segmentation in natural language](https://en.wikipedia.org/wiki/Text_segmentation).

### Test

The test reflects the problem statement:

{{< highlight haskell >}}
module EarleyExampleSpec where

import EarleyExample (grammar, NumberWord, Expected)
import qualified Text.Earley as E
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List

import Test.Hspec (Spec, hspec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))

spec :: Spec
spec =
  describe "EarleyExample" $ do
    it "returns all possible parses of number words" $ do
      parseNumberWord 1234 `shouldSatisfy` \(result, _) ->
        List.sort result ==
        List.sort [ NonEmpty.fromList "ABCD"
                  , NonEmpty.fromList "AWD"
                  , NonEmpty.fromList "LCD"
                  ]

parseNumberWord :: Integer -> ([NumberWord], E.Report Expected String)
parseNumberWord = E.fullParses (E.parser grammar) . show
{{< /highlight >}}

Note that I am using a `NonEmpty` list of `Char` because an empty
string is not a valid solution to the "number word" problem. (I
covered [`NonEmpty`] on [day 7](/blog/2015/12/07/24-days-of-hackage-2015-day-7-semigroups-nonempty-list-and-a-case-study-of-types-and-tests/).)

### Solution

The solution is just to write a grammar that tries to pick off valid
consecutive digits to make a letter. We create a production for each
possible letter that we care about, using `numberLetterFor`, combine
those productions with alternation using `asum` to get a composite
production `numberLetter`, then use that for `numberWord` which is the
grammar.

{{< highlight haskell >}}
{-# LANGUAGE RecursiveDo #-}

module EarleyExample where

import qualified Text.Earley as E
import Text.Earley ((<?>))
import Control.Applicative ((<|>))
import qualified Data.Foldable as Foldable
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))

-- | Result wanted.
type NumberWord = NonEmpty NumberLetter

-- | 'A' to 'Z'.
type NumberLetter = Char

-- | What to report for something expected.
type Expected = String

grammar :: E.Grammar r (E.Prod r Expected Char NumberWord)
grammar = mdo
  numberWord <- E.rule $
    NonEmpty.cons <$> numberLetter <*> numberWord
    <|> (:| []) <$> numberLetter
  return numberWord

numberLetter :: E.Prod r Expected Char NumberLetter
numberLetter = (Foldable.asum . map numberLetterFor) ['A'..'Z'] <?> "number"

-- | Return a production for a given letter.
--
-- 1 is 'A', 2 is 'B', .. 26 is 'Z'.
numberLetterFor :: NumberLetter -> E.Prod r Expected Char NumberLetter
numberLetterFor c = c <$ E.word (show (toNumber c)) <?> [c]

-- | 'A' is 1, ... 'Z' is 26
toNumber :: NumberLetter -> Int
toNumber c = (Char.ord c - Char.ord 'A') + 1
{{< /highlight >}}

## Conclusion

I only recently discovered the `Earley` parser library and started
using it. I'm pretty excited by its friendliness.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
