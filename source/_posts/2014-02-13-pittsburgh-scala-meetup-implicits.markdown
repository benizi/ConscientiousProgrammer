---
layout: post
title: "Pittsburgh Scala Meetup: Implicits"
date: 2014-02-13 21:45:59 -0500
comments: true
categories: 
- Scala
- Pittsburgh
- implicits
- type classes
- M*Modal
---
The [Pittsburgh Scala Meetup](http://www.meetup.com/Pittsburgh-Scala-Meetup/) met with Justin presenting on ["Implicits"](http://www.meetup.com/Pittsburgh-Scala-Meetup/events/146581402/).

<!--more-->

## Implicits

Implicits are a novel and fantastically important feature of Scala. I wish there were a comprehensive and concise single resource about them out there, but I don't actually know of one.

I've been doing stuff with implicits lately. Not long ago, I wrote a blog post that involved [using Scala implicits to avoid relying on the inherited Java `toString` method](/blog/2013/12/26/tostring-considered-harmful-part-2/). More recently, for this meetup I posted some code that I hoped someone would comment on, in which [I used implicits to try to simplify a DSL](https://github.com/franklinchen/test-specs2-matchers). I'm not sure this is the right design choice, but it was an experiment.

## Presentation

Around 7 of us showed up.

Justin did some live coding using the Scala Worksheet to demonstrate various uses of implicits.

One use is to define an "implicit function" that can convert of a value of one type to another. This can be overused badly, unfortunately.

Another is to define an "implicit parameter" for a function so that you don't have to explicitly pass a parameter to the function, if there is an implicit value in scope.

Scala 2.10 greatly improved implicits by encapsulating the "conversion" pattern by means of an "implicit class". Strangely, Scala Worksheet seemed to get confused when we played around and defined an implicit class that also had an implicit parameter. This led to some interesting detective work as we examined the generated JVM byte code to figure out what was going on! I thought it was useful for us to dig into this level; often it is useful to understand what something compiles to in order to better understand and appreciate a high-level language construct.

Finally, we briefly discussed the type class pattern, which is arguably the most important use of implicits. This was too big a topic to get into here though.

## Resources

I posted some good resources that have popped up on my radar on Scala implicits:

- A fine [tutorial on type classes in Scala](http://like-a-boss.net/2013/03/29/polymorphism-and-typeclasses-in-scala.html).
- [Scary stuff with implicits](http://typelevel.org/blog/2014/01/18/implicitly_existential.html) that I don't understand (yet).

## Conclusion

Justin gave a nice introduction to implicits. I think the live coding and experimentation is a great way to involve people in code suggestions and discussion of what things mean and do. It was a fun time.
