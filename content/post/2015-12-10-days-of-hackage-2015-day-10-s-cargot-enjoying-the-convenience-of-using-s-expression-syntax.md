---
categories:
- Haskell
- Hackage
- Lisp
- Scheme
- s-cargot
- S-expressions
- domain-specific language
- parsers
- parsec
comments: true
date: 2015-12-10T08:00:00-05:00
layout: post
title: "24 days of Hackage, 2015: day 10: s-cargot: using S-expression syntax"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 10

There are times when I'm jealous of the Lisp world.

One of those times is when defining some domain-specific language,
because in the Lisp world, the natural thing to do is to represent it
using S-expressions as the concrete syntax, and not fuss with defining
yet another special syntax, along with writing a parser for that
syntax into the abstract syntax as well as a pretty-printer from the
abstract syntax back to the concrete syntax. Maybe in the long run
users might want a special syntax that is not just S-expressions, but
for quick initial prototyping, at least, it seems worthwhile to not
commit to any special syntax and just use S-expressions. Although
there is nothing magical about S-expressions (or XML or JSON or any
other generic representation of a tree data structure), they are
particularly concise and flexible.

S-expressions are so useful that in my first job as a software
engineer in the 1990s, we actually had a C++ S-expression library for
input and output of a format that amounted to a domain-specific
language (this was before XML was invented) that was processed by many
tools (including a validator that I wrote in Standard ML).

A library on Hackage for working with S-expressions in Haskell is
[`s-cargot`](http://hackage.haskell.org/package/s-cargot). There have
been many others, but most of them have gone sadly unmaintained,
whereas this one is new and comes with bells and whistles.

Today I'll give an example of how to use this library, in the context
of a problem domain in which having a concrete syntax is important.

<!--more-->

## Installation

We need to add `s-cargot` to `stack.yaml`:

{{< highlight yaml >}}
- s-cargot-0.1.0.0
{{< /highlight >}}

## The task: symbolic differentiation

The task we solve here is, appropriately enough, a translation from the Lisp
(Scheme, to be precise) code for a [symbolic differentiator of
mathematical expressions](https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_sec_2.3.2) in the classic computer science textbook
["Structure and Interpretation of Computer Programs"](https://mitpress.mit.edu/sicp/). I
won't be walking through the solution here, but just focusing on some
syntax issues.

Example: given a mathematical expression such as the linear function
`5x + 7`, we want to find the symbolic derivative with respect to `x`,
to get `5`.

## Simplest syntax for the expression type

How do we model an expression and a function to compute a derivative
of an expression? Let's start with the most vanilla possible way,
which is to define a data type for expression, `Exp`, along with
ordinary alphanumeric constructors `N` (for number), `V` (for
variable), `Plus` (for sum of subexpressions), `Times` (for product of
subexpressions). A sample subset of an appropriate HSpec/QuickCheck
spec to define what we need:

{{< highlight haskell >}}
module SymbolicDifferentiation.AlphaSyntaxSpec where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times), deriv)

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))

spec :: Spec
spec =
  describe "symbolic differentiation" $ do
    prop "d/dx (x + n) == 1" $ \x n ->
      deriv (Plus (V x) (N n)) x `shouldBe` N 1
    prop "d/dx (x + y) == x, if x /= y" $ \x y ->
      x /= y ==>
      deriv (Times (V x) (V y)) x `shouldBe` V y
    prop "d/dx (a * x + b) == x" $ \a x b ->
      deriv (Plus (Times (N a) (V x)) (N b)) x `shouldBe` N a
    it "d/dx (x * y * (x + 3)) == (x * y) + y * (x + 3)" $ do
      deriv (Times (Times (V "x") (V "y"))
                   (Plus (V "x") (N 3))) "x" `shouldBe`
        (Plus (Times (V "x") (V "y"))
              (Times (V "y") (Plus (V "x") (N 3))))
{{< /highlight >}}

The syntax looks OK for simple expressions, but ugly when you have
lots of nested subexpressions, with parentheses for grouping, as in
the final artificial example.

Here is the code for a still-naive symbolic differentiator, having
only a few heuristics built in for some simplification through
rewriting (for example, adding `0` to an expression results in that
expression rather than construction of a superfluous `N 0`
subexpression):

{{< highlight haskell >}}
module SymbolicDifferentiation.AlphaSyntax where

-- | Variable in an expression.
type Var = String

-- | Expression.
data Exp
  = N Int          -- ^ number
  | V Var          -- ^ variable
  | Plus Exp Exp   -- ^ sum
  | Times Exp Exp  -- ^ product
  deriving (Show, Eq)

-- | Derivative of expression with respect to a variable.
deriv :: Exp -> Var -> Exp
deriv (N _)         _ = N 0
deriv (V v')        v = N (if v' == v then 1 else 0)
deriv (Plus e1 e2)  v = plus (deriv e1 v) (deriv e2 v)
deriv (Times e1 e2) v = plus (times e1 (deriv e2 v))
                             (times (deriv e1 v) e2)

-- | Smart constructor that simplifies while combining subexpressions.
plus :: Exp -> Exp -> Exp
plus (N 0)  e      = e
plus e      (N 0)  = e
plus (N n1) (N n2) = N (n1 + n2)
plus e1     e2     = Plus e1 e2

-- | Smart constructor that simplifies while combining subexpressions.
times :: Exp -> Exp -> Exp
times (N 0)  _      = N 0
times _      (N 0)  = N 0
times (N 1)  e      = e
times e      (N 1)  = e
times (N n1) (N n2) = N (n1 * n2)
times e1     e2     = Times e1 e2
{{< /highlight >}}

The syntax of the code, using pattern matching, looks reasonably good
to me. It's the `Exp` data construction that looks a bit ugly,
although not terrible. So we have a situation in which the implementor
of this little expression language is fairly happy, but the user is
not. In fact, we haven't even provided a way for a user outside the
system to create expressions: right now, we have an API but no parser
from a string into an expression.

## Parsing from what format?

One way to go is to write a custom parser from a custom syntax, for
example maybe write a function `fromString :: String -> Exp` such that

{{< highlight haskell >}}
fromString "5x + 7y + 20" ==
  Plus (Plus (Times (N 5) (V "x"))
             (Times (N 7) (V "y")))
       (N 20)
{{< /highlight >}}

We would also want to write a pretty-printer `toString :: Exp ->
String` such that maybe it's smart enough to go

{{< highlight haskell >}}
toString (Plus (Plus (Times (N 5) (V "x"))
                     (Times (N 7) (V "y")))
               (N 20)) == "5x + 7y + 20"
{{< /highlight >}}

It is straightforward to write such a parser using
[`parsec`](http://hackage.haskell.org/package/parsec) or the
like, but you can imagine that sometimes it might be annoying to
design the syntax for a much more complex language (including special
infix operators, precedences, scoping keywords, different kinds of
braces, etc.) and also make users learn it.

So let's assume for the purpose of this article that we have a reason
to prefer prefix-only S-expressions, just as the Lisp community does
in order to avoid all the hassles of a custom syntax.

Here are some sample QuickCheck tests to show what it is we want to be
able to do. (Note that for convenience, we are using string
interpolation through the
[`here`](http://hackage.haskell.org/package/here) package as
introduced yesterday,
[day 9](/blog/2015/12/09/24-days-of-hackage-2015-day-9-template-haskell-goodies-here-interpolate-file-embed/).)

{{< highlight haskell >}}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module SymbolicDifferentiation.SExpSpec where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))
import qualified SymbolicDifferentiation.SExp as SExp

import Test.Hspec (Spec, hspec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)

import Data.String.Here (i)

-- | Required for auto-discovery.
spec :: Spec
spec =
  describe "S-expression syntax for expression" $ do
    prop "(+ x a)" $ \a ->
      SExp.parse [i|(+ x ${a})|] `shouldBe`
        Right (Plus (V "x") (N a))

    prop "(* (+ x a) (+ y b))" $ \a b ->
      SExp.parse [i|
                     (* (+ x ${a})
                        (+ y ${b}))
                   |] `shouldBe`
        Right (Times (Plus (V "x") (N a))
                     (Plus (V "y") (N b)))

    it "(!? x y)" $
      SExp.parse "(!? x y)" `shouldBe`
        Left "\"!?\" is not a valid operator"
{{< /highlight >}}

I've included a little test to indicate that we want some kind of
error handling also. Nothing is more annoying to a user than terrible
parse error messages. We won't provide great messages here, but at
least will give an idea of how one could.

## The S-expression parser

`s-cargot` provides very flexible ways of constructing S-expression
parsers, based on what kind of syntax you want to allow (full Scheme
or not, for example), and allows hooks on many levels to support
readers as well as specifying the desired atom parser.

Some imports first:

{{< highlight haskell >}}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module SymbolicDifferentiation.SExp where

import SymbolicDifferentiation.AlphaSyntax (Exp(N, V, Plus, Times))

import qualified Data.SCargot as S
import Data.SCargot.Language.Basic (basicParser)
import Data.SCargot.Repr.WellFormed
       (WellFormedSExpr(WFSList, WFSAtom), fromWellFormed)

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Read (signed, decimal)

import Data.String.Here (i)

-- | Error when parsing.
type Error = String
{{< /highlight >}}

The main driver is a pipeline of calling an `s-cargot` parser and then
calling our own parser of an S-expression into our `Exp` type:

{{< highlight haskell >}}
-- | For simplicity, we use 'basicParser' which just treats every atom
-- as 'Text', which we parse later rather than up front.
parse :: Text -> Either Error Exp
parse text = parseOneSexp text >>= toExp

parseOneSexp :: Text -> Either Error (WellFormedSExpr Text)
parseOneSexp = S.decodeOne (S.asWellFormed basicParser)
{{< /highlight >}}

Once we have a well-formed S-expression, we can pick it apart, while
catching errors if we encounter one.

{{< highlight haskell >}}
toExp :: WellFormedSExpr Text -> Either Error Exp
toExp (WFSAtom text) = fromAtom text
toExp (WFSList [WFSAtom operatorText, sexp1, sexp2]) = do
  operator <- fromOperator operatorText
  e1 <- toExp sexp1
  e2 <- toExp sexp2
  return (operator e1 e2)
toExp list@(WFSList _) = Left [i|${list} should have exactly 3 elements|]

fromOperator :: Text -> Either Error (Exp -> Exp -> Exp)
fromOperator "+" = return Plus
fromOperator "*" = return Times
fromOperator text = Left [i|${text} is not a valid operator|]

-- | Either an integer or a variable.
fromAtom :: Text -> Either Error Exp
fromAtom text =
  case signed decimal text of
    Right (n, "") ->
      return (N n)
    Right (_, _) ->
      Left [i|extra garbage after numeric in ${text}|]
    Left _ ->
      return (V (Text.unpack text))
{{< /highlight >}}

## Pretty-printing

And we get pretty-printing for free from `s-cargot` if we turn our
`Exp` into an S-expression first. I won't show the details, but you
can use the basic S-expression printer or customize it with a lot
options including indentation strategy. Let's just use the basic.

A sample test for our `SExp.prettyPrint`:

{{< highlight haskell >}}
    prop "pretty-printing" $ \a b ->
      SExp.prettyPrint (Times (Plus (V "x") (N a))
                              (Plus (V "y") (N b))) `shouldBe`
       [i|(* (+ x ${a}) (+ y ${b}))|]
{{< /highlight >}}

The code:

{{< highlight haskell >}}
fromExp :: Exp -> WellFormedSExpr Text
fromExp (N n) = WFSAtom (Text.pack (show n))
fromExp (V x) = WFSAtom (Text.pack x)
fromExp (Plus e1 e2) = WFSList [WFSAtom "+", fromExp e1, fromExp e2]
fromExp (Times e1 e2) = WFSList [WFSAtom "*", fromExp e1, fromExp e2]

prettyPrint :: Exp -> Text
prettyPrint =
  S.encodeOne (S.setFromCarrier fromWellFormed (S.basicPrint id))
  . fromExp
{{< /highlight >}}

## Summary of S-expression parsing and pretty-printing

So there you have it: with a little bit of boilerplate you can get an
experience similar to that of working in Lisp. Note that with even
clever Template Haskell work with quasiquotation you could go further
than the pure text templates we've used for convenience, and create
pattern templates as well.

We didn't have to write a traditional parser, and we were able to
separate well-formedness from further processing. This is really
useful in many contexts: in my experience, the multi-level error
checking makes good error messages easier to create. Also, not
discussed here is how S-expressions can also help with prediction and
completion.

## Optional further notes on syntax for domain-specific languages

I wanted to point out that for some problem domains, such as this one
that happens to be mathematical, it is sometimes popular to make
things look mathematical. I've deliberately presented it without that
attempt first, but now I'll show some syntactic variants.

### Alphanumeric identifiers as operators

First, one can use operator syntax with backticks and precedence
levels:

{{< highlight haskell >}}
module SymbolicDifferentiation.AlphaOperatorSyntax where

-- | Variable in an expression.
type Var = String

-- | Precedences for our expression constructors.
infixl 6 `Plus`
infixl 7 `Times`

-- | Expression.
data Exp
  = N Int          -- ^ number
  | V Var          -- ^ variable
  | Plus Exp Exp   -- ^ sum
  | Times Exp Exp  -- ^ product
  deriving (Show, Eq)

-- | Derivative of expression with respect to a variable.
deriv :: Exp -> Var -> Exp
deriv (N _)         _ = N 0
deriv (V v')        v = N (if v' == v then 1 else 0)
deriv (Plus e1 e2)  v = deriv e1 v `plus` deriv e2 v
deriv (Times e1 e2) v = e1 `times` deriv e2 v
                        `plus`
                        deriv e1 v `times` e2

-- | Precedences for our expression "smart" constructors.
infixl 6 `plus`
infixl 7 `times`

-- | Smart constructor that simplifies while combining subexpressions.
plus :: Exp -> Exp -> Exp
N 0  `plus` e    = e
e    `plus` N 0  = e
N n1 `plus` N n2 = N (n1 + n2)
e1   `plus` e2   = e1 `Plus` e2

-- | Smart constructor that simplifies while combining subexpressions.
times :: Exp -> Exp -> Exp
N 0  `times` _    = N 0
_    `times` N 0  = N 0
N 1  `times` e    = e
e    `times` N 1  = e
N n1 `times` N n2 = N (n1 * n2)
e1   `times` e2   = e1 `Times` e2
{{< /highlight >}}

Note that nothing has really changed, except the use of infix
function definitions and calls, and use of precedence to remove
parentheses, such as in the big expression for the derivative of a
product of expressions. But clarity is starting to be lost, for those
not already familiar with the problem domain and conventions.

### Symbolic identifiers as operators

One can go further and use symbolic identifiers in place of the
backticked alphanumeric operators. This is where many of us start
wondering what is going on:

{{< highlight haskell >}}
module SymbolicDifferentiation.OperatorSyntax where

-- | Variable in an expression.
type Var = String

-- | Precedences for our expression constructors.
infixl 6 :+:
infixl 7 :*:

-- | Expression.
data Exp
  = N Int        -- ^ number
  | V Var        -- ^ variable
  | Exp :+: Exp  -- ^ sum
  | Exp :*: Exp  -- ^ product
  deriving (Show, Eq)

-- | Derivative of expression with respect to a variable.
deriv :: Exp -> Var -> Exp
deriv (N _)       _ = N 0
deriv (V x )      y = N (if x == y then 1 else 0)
deriv (e1 :+: e2) v = deriv e1 v .+. deriv e2 v
deriv (e1 :*: e2) v = e1 .*. deriv e2 v
                      .+.
                      deriv e1 v .*. e2

-- | Precedences for our expression "smart" constructors.
infixl 6 .+.
infixl 7 .*.

-- | Smart constructor that simplifies while combining subexpressions.
(.+.) :: Exp -> Exp -> Exp
N 0  .+. e    = e
e    .+. N 0  = e
N n1 .+. N n2 = N (n1 + n2)
e1   .+. e2   = e1 :+: e2

-- | Smart constructor that simplifies while combining subexpressions.
(.*.) :: Exp -> Exp -> Exp
N 0  .*.  _   = N 0
_    .*. N 0  = N 0
N 1  .*. e    = e
e    .*. N 1  = e
N n1 .*. N n2 = N (n1 * n2)
e1   .*. e2   = e1 :*: e2
{{< /highlight >}}

Depending on your taste, you might find that this funny syntax makes
the tests look nicer:

{{< highlight >}}
spec :: Spec
spec =
  describe "symbolic differentiation" $ do
    prop "d/dx (x + n) == 1" $ \x n ->
      deriv (V x :+: N n) x `shouldBe` N 1
    prop "d/dx (x + y) == x, if x /= y" $ \x y ->
      x /= y ==>
      deriv (V x :*: V y) x `shouldBe` V y
    prop "d/dx (a * x + b) == x" $ \a x b ->
      deriv (N a :*: V x :+: N b) x `shouldBe` N a
    it "d/dx (x * y * (x + 3)) == (x * y) + y * (x + 3)" $ do
      deriv (V "x" :*: V "y" :*: (V "x" :+: N 3)) "x" `shouldBe`
        (V "x" :*: V "y") :+: (V "y" :*: (V "x" :+: N 3))
{{< /highlight >}}

All this looks kind of cute if you're used to it, but I think it can
look really weird otherwise; and I know that many non-Haskellers
seeing this before seeing the vanilla way get the wrong idea that you
have to write this way in Haskell and turn away in disgust and
confusion. This is why I showed the vanilla way first, and show this
only to illustrate that there are libraries out there that do try to
be very suggestive in symbolic operator usage, and also that there is
nothing special going on here: it's just a different way of saying
exactly the same thing as the first version, with the second version
(backticked alphanumerics as operators) being a transitional step
toward this third version. It's good to know about all three variants,
regardless of which one you prefer to read and write.

But what about the S-expression route, which is to decouple the user
(represented by the tests) side of things from the abstract syntax and
the Haskell syntax? My intuition is that there are real benefits in at
least providing an alternate S-expression syntax for whatever other
concrete syntax is made available.

## Conclusion

S-expressions are a time-honored way of representing data. The
`s-cargot` library comes with ways to build custom S-expression
parsers and pretty-printers and also comes with useful defaults.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
