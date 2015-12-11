---
categories:
- Haskell
- Hackage
- recursion
- monad-loops
comments: true
date: 2015-12-11T08:00:00-05:00
layout: post
title: "24 days of Hackage, 2015: day 11: monad-loops: avoiding
writing recursive functions by refactoring"
---
## Table of contents for the whole series

A table of contents is at the top of the article for [day 1](/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/).

## Day 11

I often dislike writing recursive functions. Recursion is a form of
`goto` and therefore recursion is, conceptually, the assembly
language low-level infrastructure of functional programming, a
fundamental building block but not necessarily something I want to see
every day.

An analogy: imperative programming in the 1960s was altered
significantly when brand new control structures such as `for` and
`while` loops changed the face of imperative programming by removing
the boilerplate of using the low-level `goto` everywhere; `for` and
`while` are the higher-order control structures of imperative programming.

In functional programming, recursion boilerplate is avoided through
writing and using higher-order functions such as `map`,`filter`,
`foldr`, and `traverse`. The cool thing is that in functional
programming, these are ordinary user-space functions, whereas in
imperative programming, the control structures are built into the
language and it's typically not so easy to invent your own.

Why would you want to define control structures yourself? I'll show
examples and a useful little library
[`monad-loops`](https://hackage.haskell.org/package/monad-loops) that
you can use in order to avoid reinventing the wheel.

<!--more-->

## A sample task

Let's write a function to simulate a user login process, in which

- the user is prompted to enter a password
- a loop is entered in which
    - a guess is read in
    - if it is not correct, we prompt again and loop
    - if it is correct, we exit the loop
 - we print congratulations

Here is a literal translation of the pseudocode into Haskell using
recursion for the loop:

{{< highlight haskell >}}
logIn :: IO ()
logIn = do
  putStrLn "% Enter password:"
  go
  putStrLn "$ Congratulations!"

  where
    -- Use recursion for loop
    go = do
      guess <- getLine
      if guess /= "secret"
        then do
          putStrLn "% Wrong password!"
          putStrLn "% Try again:"
          go
        else
          return ()
{{< /highlight >}}

Do you like this code? Me, I'm annoyed by having to write that
"helper" recursive function just to write a loop.

### Comparision with typical imperative style

Compare with typical Python code:

{{< highlight python >}}
def log_in():
  print "% Enter password:"
  while raw_input() != 'secret':
    print "% Wrong password!"
    print "% Try again:"
  print "% Congratulations!"
{{< /highlight >}}

There's a reason
[`while` loops](https://en.wikipedia.org/wiki/While_loop) were
invented in the 1960s for imperative programming. It was to improve
the expression of *control flow patterns* of this form. It would be
backwards to return to programming entirely in `goto` style. (However,
this does *not* mean `goto` and recursion are bad. Knuth famously
wrote an
[article in 1974](http://c2.com/cgi/wiki?StructuredProgrammingWithGoToStatements)
defending the careful use of `goto` against what he correctly noted
was dogma that went to far against `goto`. The same could be said
against any anti-recursion dogma.)

When I teach Haskell, I get embarrassed when people wonder what's so
"wrong" about Haskell that you can't write straightforward code like
the Python code above, but have to pull out the recursion thing just
to do something simple. The dilemma I face when teaching is that the
thought in my mind is, "But, but, we can have loops too, they're just
not built into the language and you can even write your own using
higher-order functions!" while I know that many are thinking "What a
crappy ivory-tower impractical academic language, it doesn't even have
while loops", and meanwhile I also know that you can't just jump into
higher-order functions in the first hour.

### Making your own loop

The central mindset of functional programming is that if you see
boilerplate, a pattern, then it might be worthwhile to refactor into
two components:

- the abstract structure of the pattern
- the concrete instance of the problem

So `monad-loops` provides a bunch of useful combinators capturing
common control flow patterns. For example, we can use the function
`whileM_` for our problem:

{{< highlight haskell >}}
import Control.Monad.Loops (whileM_)
{{< /highlight >}}

The type of `whileM` is

{{< highlight haskell >}}
whileM_ :: Monad m => m Bool -> m a -> m ()
{{< /highlight >}}

It takes a "condition" that evaluates a `Bool` (in a monadic context
`m`), along with an arbitrary monadic "body" action to perform, to
return a "loop" (the monadic action that returns unit) which keeps
testing the condition and performing the body.

Our code rewritten:

{{< highlight haskell >}}
-- | No explicit recursion
logIn2 :: IO ()
logIn2 = do
  putStrLn "% Enter password:"
  whileM_ (do
             guess <- getLine
             return (guess /= "secret")
          ) (do
               putStrLn "% Wrong password!"
               putStrLn "% Try again:"
            )
  putStrLn "$ Congratulations!"
{{< /highlight >}}

This looks better than the explicit recursion, except for the clumsy
syntax because our loop "condition" and "body" are just ordinary
expressions and therefore need to be parenthesized to be passed into
the `whileM_` function.

Note that the
[implementation of `whileM`](https://hackage.haskell.org/package/monad-loops-0.4.3/docs/src/Control-Monad-Loops.html#whileM_)
is just ordinary Haskell code that abstracts exactly from the
structure of our original code, by pulling out the condition and
body. Functional programming is all about ["extract method"](http://refactoring.com/catalog/extractMethod.html)!

{{< highlight haskell >}}
whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = do
            x <- p
            if x
                then f >> go
                else return ()
{{< /highlight >}}

#### Improving the syntax

I've warned in earlier posts against getting too clever with syntax,
so I deliberately presented "normal" syntax above for calling the
`whileM_` function, to show that it really is normal code, nothing
magical. But for those of you who are not already deeply embedded in
Haskell shortcut idioms, below are some rewrites into "fancier"
syntax, but *do exactly the same thing*.

The first step to avoid a lot of parenthesizing of expressions is to
use the `$` function application operator, a trick to enable removing
the parentheses of the final expression (the one serving as loop
"body"):

{{< highlight haskell >}}
-- | With $ syntax.
logIn3 :: IO ()
logIn3 = do
  putStrLn "% Enter password:"
  whileM_ (do
             guess <- getLine
             return (guess /= "secret")
          ) $ do
    putStrLn "% Wrong password!"
    putStrLn "% Try again:"
  putStrLn "$ Congratulations!"
{{< /highlight >}}

Another step is to use some lifting of operations into the monad using
`liftM`:

{{< highlight haskell >}}
import Control.Monad (liftM)

-- | With lifting.
logIn4 :: IO ()
logIn4 = do
  putStrLn "% Enter password:"
  whileM_ (liftM (\guess -> guess /= "secret") getLine) $ do
    putStrLn "% Wrong password!"
    putStrLn "% Try again:"
  putStrLn "$ Congratulations!"
{{< /highlight >}}

The cost is that you have to understand lifting and the
`$` operator.

If you're willing to pay another cost, that of using operator
sectioning and the symbolic `fmap` operator `<$>`, here's a final
version:

{{< highlight haskell >}}
-- | With operator sectioning and <$>.
logIn5 :: IO ()
logIn5 = do
  putStrLn "% Enter password:"
  whileM_ ((/= "secret") <$> getLine) $ do
    putStrLn "% Wrong password!"
    putStrLn "% Try again:"
  putStrLn "$ Congratulations!"
{{< /highlight >}}

This looks a lot like the Python code, except it is full of syntax
that is completely mysterious to a newcomer to Haskell.

## Another task: reading and collecting lines

One last example of refactoring away recursion:

Suppose you want to write an `IO` action for a console application
that reads lines from user input until encountering "quit", and you
want to collect all these lines (but not "quit") into a list:

{{< highlight haskell >}}
readLinesUntilQuit :: IO [String]
{{< /highlight >}}

Here's a sample interactive session:

{{< highlight console >}}
> readLinesUntilQuit
hello
lovely
world!
quit
{{< /highlight >}}

Result:

{{< highlight console >}}
["hello","lovely","world!"]
{{< /highlight >}}

Here's a straightforward implementation, using recursion.

{{< highlight haskell >}}
readLinesUntilQuit :: IO [String]
readLinesUntilQuit = do
  line <- getLine
  if line /= "quit"
    then do
      -- recursive call, to loop
      restOfLines <- readLinesUntilQuit
      return (line : restOfLines)
    else return []
{{< /highlight >}}

This is not unreadable, but there is definitely a lot of boilerplate
going on:

- a condition
- recursion
- collection stuff into a list

### Refactoring recursion away

We'll use `unfoldM`:

{{< highlight haskell >}}
unfoldM :: Monad m => m (Maybe a) -> m [a]
{{< /highlight >}}

All the work is in the argument, which we extract into its own
definition:

{{< highlight haskell >}}
-- | No explicit recursion.
readLinesUntilQuit2 :: IO [String]
readLinesUntilQuit2 = unfoldM maybeReadLine

-- | Read a single line and check whether it's "quit".
maybeReadLine :: IO (Maybe String)
maybeReadLine = do
  line <- getLine
  return (if line /= "quit"
          then Just line
          else Nothing)
{{< /highlight >}}

We can go further in this refactoring, because `maybeReadLine`
intermingles both `IO` and a pure conditional check of the input
line. **Parenthesized expressions are often a code smell suggesting a
refactoring opportunity.**

{{< highlight haskell >}}
readLinesUntilQuit3 :: IO [String]
readLinesUntilQuit3 = unfoldM (notQuit <$> getLine)

notQuit :: String -> Maybe String
notQuit line =
  if line /= "quit"
    then Just line
    else Nothing
{{< /highlight >}}

I like this last version because it decouples the loop, the condition,
and the fundamental IO action bringing in more information to use.

## A note on testing

If you've been following this entire Days of Hackage article series,
you may be wondering why I didn't provide HSpec tests and walk through
in test-driven development style. The reason is that I didn't want to
get into how one would "mock out" IO in order to write tests that
simulate user activity.

## Conclusion

I gave two example refactorings of code to avoid explicit recursion in
favor of using combinators from `monad-loops`. I hope the exploration
of refactoring as well as of syntax is useful to those not already
familiar with these techniques and idioms. Check out the whole library
for more combinators you can use.

## All the code

All my code for my article series are at
[this GitHub repo](https://github.com/FranklinChen/twenty-four-days2015-of-hackage).
