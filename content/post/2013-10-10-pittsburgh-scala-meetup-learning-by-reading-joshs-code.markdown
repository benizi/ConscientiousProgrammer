---
layout: post
title: "Pittsburgh Scala Meetup: learning by reading Josh's code!"
date: 2013-10-10T23:09:33-05:00
url: "blog/2013/10/11/pittsburgh-scala-meetup-learning-by-reading-joshs-code/"
comments: true
categories: 
- Scala
- SBT
- Pittsburgh
- M*Modal
---
The [Pittsburgh Scala Meetup](http://www.meetup.com/Pittsburgh-Scala-Meetup/) met to [learn by hacking](http://www.meetup.com/Pittsburgh-Scala-Meetup/events/135567132/). There was a change of plan because Josh couldn't make it to the meeting, so instead of a presentation by him, we got a link to his GitHub repository for an implementation of an [interactive Web-based tic-tac-toe game using Play](https://github.com/jsuereth/tic-tac-toe).

Sometimes interesting things happen when plans are changed.

<!--more-->

## Reading Josh's code

Since Josh wasn't around, we decided to study his code, and use it as the basis of discussion of Scala language features and idiomatic style, making sure that all of us understood what the code was doing. This turned out to be a surprisingly useful exercise, very participatory by everyone.

Justin took charge of an SBT session as we played with modifying the code and figuring out what things did. I shared some tips on using SBT in "trigger mode", which some had not known about. While experimenting, we ran into interesting Scala gotchas involving `def` and `val` in classes that mix in traits. We also had useful discussions on coding style, such as point-free style and use of underscores in closures, and converting between curried and uncurried functions. My personal point of view is that I prefer to be more explicit rather than more concise, to improve clarity, but much does depend on assumptions about people's prior knowledge. Josh was writing this code for only himself, and the complete application was actually not meant to showcase the tic-tac-toe game logic anyway, but the use of Play.

## Conclusion

I thought it was a really useful session in which we all helped one other get up to speed on various Scala language features or standard library APIs. We all learned something new, and we figured out Josh's code, and collected questions to ask him when he comes back!

