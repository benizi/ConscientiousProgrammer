---
layout: post
title: "Learning about Parasail: a new parallel programming language"
disqus_identifier: "http://ConscientiousProgrammer.com/blog/2012/10/17/learning-about-parasail-a-new-parallel-programming-language/"
disqus_url: "http://ConscientiousProgrammer.com/blog/2012/10/17/learning-about-parasail-a-new-parallel-programming-language/"
date: 2012-10-17T20:39:15-04:00
comments: true
categories:
- Parasail
- parallelism
- concurrency
- Carnegie Mellon University
- Ada
- types
- Tucker Taft
- Guy Blelloch
- NESL
- Standard ML
- Haskell
---
I noticed an announcement for a talk to be given at CMU for the [ISR](http://www.isri.cmu.edu/) by visitor Tucker Taft, ["ParaSail: A Pointer-Free Path to Object-Oriented Parallel Programming"](http://www.cs.cmu.edu/afs/.cs.cmu.edu/Web/copetas/Posters/ISR-Taft12.pdf), and decided to attend.

I'd recognized [Tucker Taft](http://en.wikipedia.org/wiki/User:Optikos/S._Tucker_Taft)'s name because decades ago (and [now still](http://www.adacore.com/company/about/executive-team/)), he was a very prominent member of the [Ada programming language](http://en.wikipedia.org/wiki/Ada_%28programming_language%29) community, being one of the primary designers of the extensions to the original Ada 83. I never actually used Ada 95, but was following it a little bit because in 1995 I had to maintain some Ada 83 code at work, so I was curious where Ada was going. (I haven't used Ada since leaving that job in 1997.)

So I was curious what an Ada guy had in mind as for one of the next steps in programming language design (parallelism), and why a new language and how it would be informed by an Ada mindset.

<!--more-->

## Parasail, and why a new language anyway?

The new language Taft has been working on is called Parasail and [its Web site is really a blog](http://parasail-programming-language.blogspot.com/).

Why a new language, and not, say, libraries and idioms to bolt onto an existing language? Because to make parallelism "easy" to program, and correctly, language support is the way to go, leveraging types and compiler knowledge to help programmers.

This is not a new thought, of course. Parallel languages have been designed and implemented for decades. But none can be said to have truly caught on universally in practice. For example, as a graduate student in the late 1990s, I learned and used [Guy Blelloch](http://www.cs.cmu.edu/~guyb/)'s [NESL](http://www.cs.cmu.edu/~scandal/nesl.html), based on the functional language [Standard ML](http://en.wikipedia.org/wiki/Standard_ML), and I briefly participated in exploring bringing the ideas from NESL back into ML itself, as part of the [PSciCo project](https://www.cs.cmu.edu/~pscico/).

[Data Parallel Haskell](http://www.haskell.org/haskellwiki/GHC/Data_Parallel_Haskell) is another example of an attempt at a very high level parallel programming language.

But these languages all come from the functional programming world.

## Mutation

Apparently the goal of Parasail is to not force programmers to go all out into functional programming. For both style and efficiency, it may be desirable to program imperatively, with mutation. So how does Parasail try to cope with all the potential problems caused by mutation?

Parasail bans pointers, hiding them from the programmer by using types. In particular, assignment is by copy, but move and swap semantics are used underneath.

## Memory management

Furthermore, garbage collection is avoided through [region-based memory management](http://en.wikipedia.org/wiki/Region-based_memory_management). This is an old idea, of course: I remember experimenting with using MLKit from the 1990s by Tofte and Talpin when it was hot off the press.

## Code examples

Taft gave some code examples to illustrate paralellization in Parasail. For example, quicksort is important because of its use of mutation.

## Other features

`return` is intentionally a sync point that introduces nondeterminism. Also, underneath, work stealing is used to handle the threads used underneath.

## Conclusion

It was interesting to see new work on parallel programming languages. Check out Parasail's Web site if you're interested in learning more.
