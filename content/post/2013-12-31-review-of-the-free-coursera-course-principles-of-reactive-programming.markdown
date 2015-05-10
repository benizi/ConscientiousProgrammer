---
layout: post
title: "Review of the free Coursera course \"Principles of Reactive Programming\""
date: 2013-12-31T21:29:39-05:00
comments: true
categories: 
- reactive
- Scala
- Java
- monads
- macros
- future
- promise
- async
- ScalaCheck
- RxScala
- RxJava
- Akka
- actors
---
At long last, I have officially completed the free Coursera course ["Principles of Reactive Programming"](http://www.coursera.org/course/reactive), receiving my "Statement of Accomplishment".

This was an intense course, a lot of work, actually, and made the last two month s of my life (November and December) challenging as I juggled many activities. But it was worth the effort. I would definitely recommend this course to anyone who has completed the introductory course ["Principles of Functional Programming in Scala"](http://www.coursera.org/course/progfun) (or has the equivalent background). (See my [review of that course as offered in fall of 2012](http://franklinchen.com/blog/2012-11-15-review-of-courseras-fall-2012-functional-programming-principles-in-scala/).)

I took this course along with a bunch of local friends who are also members of the [Pittsburgh Scala Meetup](http://www.meetup.com/Pittsburgh-Scala-Meetup/).

<!--more-->

## What is "Reactive"?

"Reactive" is a fairly new buzzword, popularized recently by the [Reactive Manifesto](http://www.reactivemanifesto.org/) to urge consideration of four traits modern applications may need to have:

- responsive
- scalable
- resilient
- event-driven

## Instructors and topics

Each instructor was responsible for covering a key technology.

### Martin Odersky

Martin Oderksy, the inventor of Scala and instructor for the previously mentioned "Principles of Functional Programming in Scala" course, focused on covering more advanced usage of Scala.

He immediately introduced monads, which are key to many of the concepts and libraries used throughout this course. Scala's support for monads through the syntax of for-comprehensions is very helpful. He introduced the property-based testing framework [ScalaCheck](http://www.scalacheck.org/) (which uses monads for generation of data), and the first assignment involved working with the framework. (By the way, I gave a [talk on property-based testing using ScalaCheck](http://franklinchen.com/blog/2013/04/11/my-pittsburgh-scala-meetup-talk-on-property-based-testing-using-scalacheck/) eight months ago.)

Then he discussed modeling event simulation in Scala using mutable objects. I felt that this unit was not in the spirit of the others, because there was so much uncontrolled mutable state running around. My friends and I agreed that the assignment for this unit was burdensome and not really useful. I thought to myself, in fact, that I would never solve the problems in this way.

### Erik Meijer

Erik Meijer is a very energetic and humorous speaker. I really enjoyed his lectures.

In his first unit, he introduced asynchronous programming using Scala's [`Future` monad (backed by a `Promise`)](http://docs.scala-lang.org/overviews/core/futures.html), as well as the cool new [macro-based `async`/`await` library](https://github.com/scala/async), which greatly simplifies writing code. Our assignment, which was instructive and interesting, was to implement a baby version of Node.js in Scala, ha!

Then he introduced [`RxScala`](https://rxscala.github.io/), a Scala adaptor for [`RxJava`](https://github.com/Netflix/RxJava), which is a Java port of Microsoft's "reactive extensions" `Rx` framework. I enjoyed this unit a lot, and appreciated the great documentation available (the "marble diagrams" are particularly useful). One nitpick I suppose I must have with his presentation is that I'm not sure that, for a non-theory-oriented audience, the discussions of "duality" were necessary. The assignment was very practically-oriented, gathering data asynchronously and displaying it and allowing user interaction. It was enjoyable and showcased the power of using a framework like Rx.

### Roland Kuhn

Roland Kuhn covered the most complex section of the course, introducing actors by means of the [Akka](http://akka.io/) framework.

The first assignment was fairly straightforward, on implemented a distributed binary tree with actors.

The second assignment I found quite difficult, and not only because it was crunch time at the end of December before holiday season! It was a distributed key-value store with multiple levels of possible failure. There is no way I would have successfully and correctly completed this assignment without the helpful discussions I found on the online Coursera forums. To really figure out what was going on required (for me) turning on logging in various places and writing a lot of tests using Akka's [`TestKit`](http://doc.akka.io/docs/akka/snapshot/scala/testing.html). I realized eventually that my code was ugly and not entirely clean and idiomatic, but it was definitely a worthwhile learning experience on a realistic problem to solve.

## Conclusion

I learned quite a lot from this course. It is a very practical course on "reactive" programming. I felt after completing this course that I was ready to use the concepts and technologies covered in real problems. I am grateful to the instructors (who were active on the forums, by the way) for sharing their theoretical and practical knowledge as a free Coursera course.
