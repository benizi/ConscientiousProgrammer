---
layout: post
title: "Pittsburgh Scala Meetup: Introduction to Reactive"
date: 2014-01-09 21:29:20 -0500
comments: true
categories: 
- Scala
- Pittsburgh
- SBT
- futures
- Go
- Clojure
- core.async
- back pressure
- iteratees
- Akka
- Typesafe Activator
- Play
- actors
- channels
- reactive
- M*Modal
---
The [Pittsburgh Scala Meetup](http://www.meetup.com/Pittsburgh-Scala-Meetup/) met with Josh presenting an ["Introduction to Reactive"](http://www.meetup.com/Pittsburgh-Scala-Meetup/events/146581352/).

It was great.

<!--more-->

## Pre-meetup dinner

A couple of us (Justin, Josh, Chris, me) had an early dinner at Everyday Noodles before the meetup. Very filling!

## Turnout

Ten of us showed up for this meeting.

## Presentation

### What is "reactive" anyway?

In case you didn't hear about it in the media, the term "reactive" has been popularized in recent months, and I [reviewed the free Coursera course "Principles of Reactive Programming"](/blog/2013/12/31/review-of-the-free-coursera-course-principles-of-reactive-programming/) that some of us in the Pittsburgh Scala group just completed, so check out my post for more on "reactive".

### Futures

Josh did a live SBT session reviewing futures and promises.

He noted that futures have limitations as a component of reactive systems. (This had become apparent when [I started using futures last year in my personal projects](http://franklinchen.com/blog/2013/03/25/openhack-pittsburgh-exploring-scala-odds-and-ends/) and then [for work](/blog/2013/08/02/pittsburgh-ruby-python-social/).) By themselves, futures do not support cancellation or [back pressure](http://en.wikipedia.org/wiki/Back_pressure). Futures are a low-level, limited mechanism that are the right tool only if you have a pipeline all the way through forward, no back channel.

An alternative to futures, of course, is to go all out and use Akka actors. But the drawback I've found with that is that this is a heavyweight mechanism for problems that don't need the full power of actors. I've wanted something more structured and lightweight than using actors directly. I find it tricky to program using actors, because it is very easy to start writing spaghetti code that is spread out everywhere (which is what happened to me in the final project for the Coursera course).

It turns out, according to Josh, that Typesafe is working on precisely this problem! Nice. He'll tell us more when it comes out.

#### (Update of 2014-07-10)

Half a year later, the promise was realized, as the Pittsburgh Scala Meetup had Josh [gave a talk on the new API called Reactive Streams](/blog/2014/07/10/pittsburgh-scala-meetup-reactive-streams/)!

### Play

Josh demonstrated [Typesafe Activator](http://typesafe.com/activator), the cool new Web browser-based platform for using Scala. He showed an app using Play that looks at blogs simultaneously and uses `recoverWith`.

## Random other questions and discussions

What I like about the Pittsburgh Scala Meetup is that even when we have a presentation, discussion often wanders off to related topics (or unrelated topics). Anything goes, and I usually learn a lot.

Someone asked about support for channels as in Go or Clojure's [`core.async`](https://github.com/clojure/core.async). Josh said they're working on this sort of thing with support for back pressure.

Someone asked about how to connect [iteratees](http://www.playframework.com/documentation/2.0/Iteratees) with actors. Josh said, you can feed from an iteratee to an actor, and then just let the actor do stuff. However, there is some controversy about the whole iteratee thing in Play.

## Conclusion

This was a great Pittsburgh Scala Meetup session. I felt that a lot of questions that had built up in my mind after having gotten into reactive programming are being addressed with continuing work.
