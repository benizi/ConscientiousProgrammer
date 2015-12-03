---
layout: post
title: "Why programming puzzlers make me sad"
date: 2014-07-17T23:01:52
comments: true
categories:
- puzzlers
- Perl
- C
---
I recently saw a
["guess what this does" article](http://blog.plover.com/2014/07/17/)
on a blog I follow, and this post presents mysterious Perl code for
which the reader is asked to guess what it does:

{{< highlight console >}}
perl -le 'print(two + two == five ? "true" : "false")'
{{< /highlight >}}

I looked at it briefly, got a headache, and didn't even want to solve
it. This despite using Perl as one of my main programming languages
from 1993-2010 and considering myself fairly proficient at Perl.

Programming puzzlers just in general make me sad.

<!--more-->

## I know every programming language has quirks

It is a human reality, given human imperfection, that the languages we
invent have quirks of some kind. You might expect that, unlike natural
languages like English and Chinese, which have the burden of no
centralized design and hundreds or thousands of year of history and
random evolution, computer languages would be designed up front to
avoid ambiguity and just plain confusion. But the human desire to make
some things "easy" through clever defaults or implicit assumptions
always results in an invented computer language that has
irregularities or unexpected behavior somewhere.

## The strange love of puzzlers

Still, it makes me sad when I see "puzzlers", and especially how they
are used. For some reason, in some circles, it is considered a sign of
intelligence or competence to be able to decode strange
puzzlers. Many academic homework assignments and exams tend to focus
on weird puzzlers as a way of supposedly testing proficiency in
programming. Java certification exams and job interview questions
often throw in puzzlers. I deplore this situation.

I understand that in specialized circumstances, you would want to
value someone who was really good at puzzlers: someone who could write
a conforming compiler for a language, diagnose strange bugs, etc. But
that is not what most of us do or need to do. And too many puzzlers
makes non-programmers wince and stay away from a field they consider
to be pointlessly capricious.

## When to understand puzzlers

The exception I make is when a "puzzler" is not actually rare code,
but typical code that has some kind of mistake. Some languages have
more of these puzzlers that are actually critical to understand in
order to be functional at working with code in them. For example,
almost all normally used features of C could be considered puzzlers!

## Languages with "puzzler" books

Many languages have spawned "puzzler" books or sites. You can look
them up for yourself if you are curious about puzzlers in your
favorite languages. I won't list any here because I wouldn't know
where to stop. I didn't mean to pick on Perl, of course, when writing
this article.

## An idea: how about learning from all the puzzlers?

I have an idea: how about pooling together all the puzzlers in all the
languages that currently exist, classifying the language misfeatures
that resulted in those puzzlers, and coming up with a plan to

- teach the puzzlers in a systematic polyglot way
- fix as many of these puzzlers as possible in the languages (I expect
this to be very difficult because of compatibility needs)
- write up a guide to what *not* to do in future *new* languages

**Do you think there would be value in mining existing puzzlers for
  the purpose of a systematic resource for learning about programming
  language design?**

## Conclusion

I don't like programming puzzlers. They are supposed to be funny, and
maybe sometimes they are, but often they are abused for status and
mistaken as an accurate gauge of competence.

**How do you feel about puzzlers? Do you use them for assessing your
  own or others' knowledge? Do you solve them for entertainment? Do
  you think minimizing the existence of puzzlers should be a criterion
  for design of any new programming languages?**
