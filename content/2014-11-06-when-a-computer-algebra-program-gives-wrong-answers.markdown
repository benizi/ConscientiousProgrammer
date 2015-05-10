---
layout: post
title: "When a computer algebra program gives wrong answers"
date: 2014-11-06 20:20:32 -0500
comments: true
categories:
- Mathematica
- Maple
- trust
- bugs
- testing
---
I read an interesting and disturbing report, ["The Misfortunes of a Trio of Mathematicians Using Computer Algebra Systems. Can We Trust in Them?"](http://www.ams.org/notices/201410/rnoti-p1249.pdf) published in the Notices of the American Mathematical Society. I feel that all software developers should read this and reflect on the nature of our work, when people who are not programmers depend on it.

This report was about mathematicians accidentally discovering a bug in a major computer algebra system, Mathematica.

<!--more-->

## A note about scientists and mathematicians

Scientists and mathematicians increasingly depend on computer software for their work, to explore data, analyze it, verify hypotheses, etc. It's gotten to the point where we can no longer pretend, for example, that scientists are not writing substantial computer programs, whether or not they are trained programmers (in fact, [Software Carpentry](http://software-carpentry.org/) was created by a physicist to fulfill the need of training scientists in programming).

## The discovery of the bug

A bunch of mathematicians used Mathematica to generate and test ideas they had about a mathematical hypothesis. They wanted to find counterexamples to their conjectures and found some with integer arithmetic calculations using Mathematica.

One of them happened to also be using Maple, and the results differed, so Mathematica or Maple had to be wrong. They isolated the error by *generating random test cases* and finding that Mathematica was in error.

In fact, given the *same matrix* and calling Mathematica's matrix determinant function, Mathematica would return different results!! This was a rather serious bug indeed. They found that Mathematica 7 did not have this bug, but 9 and 10 did.

## Wolfram Research's lack of responsiveness to the problem

They reported the Mathematica bug to Wolfram Research but got no useful reply, and at the next release of Mathematica, the bug was still not fixed.

There were other bugs they found as well.

Wolfram Research never gave any feedback, and does not publish a list of known bugs.

## Lessons to learn

- When there is a bug in proprietary, closed-source software, you are completely helpless. The bug may not even be acknowledged, much less fixed, and you could not fix it yourself even if you wanted to.
- There is value in having an alternative tool: without independent work using Maple, the bugs in Mathematica may never have been discovered. Diversity is good.
- All scientists should be aware that the tools they use can be buggy, and therefore computational results can only be *trusted* as much as the specific versions of software they use can be trusted.

**Does any of this surprise you? For what you rely on, how do you work around the inevitable fact that software has bugs?**
