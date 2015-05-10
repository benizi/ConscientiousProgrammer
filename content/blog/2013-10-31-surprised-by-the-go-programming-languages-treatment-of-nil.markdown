---
layout: post
title: "Surprised by the Go programming language's treatment of nil"
date: 2013-10-31 23:38
comments: true
categories: 
- Go
- nil
- C
---
{% img http://golang.org/doc/gopher/frontpage.png Go %}

So I happened to see an [announcement of Go version 1.2](http://tip.golang.org/doc/go1.2). I saw something that disturbed me, having to do with `nil`, a ["favorite" topic of mine](/blog/2013/06/29/nil-non-determinism-exceptions/).

<!--more-->

## What I know about Go

I don't currently use the [Go programming language](http://golang.org/), although there actually is a local Pittsburgh Go programming meetup group, the [Go Steel Programmers](http://www.meetup.com/Go-Steel-Programmers/), whose meetings I have never attended.

Some months ago I did, out of curiosity (and respect for any new language that I hear about people actually using to get stuff done), work through the [tour of Go](http://tour.golang.org/) tutorial, to learn about the Go language. I installed packages on my machine, and wrote some compiling and running programs. I know just enough that I could code in Go for some project if I wanted to.

## `nil`

What caught my eye in the Go 1.2 announcement was a [note about changes in the semantics regarding `nil`](http://docs.google.com/document/d/14DgGJKGQeBTNJDXo3YxnlSwv7ouRqvj7BMmZw17vWV0/pub).

For the record, I don't believe any new programming languages should be invented that have the `nil` construct, [Hoare's "billion dollar" mistake](http://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare). I've already [previously given a talk](http://franklinchen.com/blog/2012/09/06/my-pittsburgh-ruby-talk-nil/) about why, so I won't repeat the arguments here.

But I think Russ Cox's note about `nil` checks speaks for itself.

It said that Go 1.2 tightens things up so that various uses (directly or indirectly) of `nil` will cause a *runtime panic rather than silently producing an unusable pointer*.

Read that again: until Go 1.2, you could get *silent* bad behavior, an *unusable* pointer. This from a language that purports to be "statically typed", improve on C, and provide [*memory safety guarantees*](http://golang.org/doc/faq#unions).

Before Go 1.2, a chain of code involving a `nil` could result in behavior of which [Russ Cox](http://swtch.com/~rsc/) wrote:

{% blockquote %}
The current behavior is at best merely historical accident; it was definitely not thought through or discussed.
{% endblockquote %}

There is also a note about a particular special case:

{% blockquote %}
(it seemed like a good idea at a time)
{% endblockquote %}

You can read the [whole document about `nil`](http://docs.google.com/document/d/14DgGJKGQeBTNJDXo3YxnlSwv7ouRqvj7BMmZw17vWV0/pub) yourself. You will find that it is still not a formal spec, but more a rationale of various special cases and possible implementation details.

## C culture

Go arose from C culture. It was invented by those in C culture who wanted an improvement over the known problems of C. Unfortunately, I perceive it as today's C, in the same way that the C invented in the 1970s was born with the problems that one could have avoided already at that very time.

## Conclusion

The Go language continues to evolve, which is good, but I was surprised that the full ramifications of something as dangerous as rampant `nil` were not thought about up front.
