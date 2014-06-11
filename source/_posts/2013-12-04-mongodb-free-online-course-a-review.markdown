---
layout: post
title: "MongoDB free online course: a review"
date: 2013-12-04 22:03:55 -0500
comments: true
categories: 
- MongoDB
- NoSQL
- Java
- Scala
- Casbah
- Node
- JavaScript
---
{% img http://www.mongodb.com/sites/all/themes/bonsai/logo.png MongoDB logo %}

I finally finished a two-month [free online course on MongoDB](http://education.mongodb.com/), given by [MongoDB, Inc](http://www.mongodb.com/).

This is a review of the specific course numbered M101J, "MongoDB for Java Developers", but it should apply to all thee introductory developer courses on MongoDB in any language, because actually, I had originally signed up months earlier for their original course (in JavaScript), and the content is largely the same (I had gotten busy and dropped that course).

<!--more-->

## Why learn MongoDB?

I had first heard of MongoDB over two years ago, at a meeting of the (now defunct) [original Pittsburgh JavaScript meetup group](http://www.meetup.com/Pittsburgh-JavaScript-Developers/), ["In-depth look at Node.js and NoSQL"](http://www.meetup.com/Pittsburgh-JavaScript-Developers/events/25229441/).

MongoDB has become very popular; as far as I can tell, this is because of extreme marketing efforts, as well as the fact that it is very easy to get started doing stuff with it, being a document-oriented NoSQL database requiring no schema.

I took this course in part to expose myself to some NoSQL technology and also in part because I anticipated needing to use something like it. In fact, it turns out [I did use it](/blog/2013/08/02/pittsburgh-ruby-python-social/), heavily, in exactly the kind of use case that works fine with it.

## Why did I take the Java version of the course?

I took the "M101J: MongoDB for Java Developers" version of the course because I anticipated writing code in Scala to access MongoDB. In fact, during the course, I ended up using the official Scala driver [Casbah](https://github.com/mongodb/casbah) when possible.

## Summary of the course

A lot of the course actually involved using the JavaScript-based MongoDB shell, which makes sense because it is easy to explore data that way. Since everything is JSON-like, there's no real escaping JavaScript if you're working with MongoDB.

The video lecture/quiz/assignment format is fairly standard for MOOCs, and worked fairly well.

The course was more work than I expected, because it lasted so long, two months.

The ad hoc nature of MongoDB's API (including the Java-based one) tended to bother me throughout.

The weird CRUD syntax, shoehorning everything into a JSON representation, took some getting used to.

The way to specify indexes and to evaluate your guesses about whether they are actually working the way you expected was to look at funny values in JSON. I found this low-level and annoying.

The aggregation API seemed even weirder. Overall, everything had a loose "dynamic" feel to it: this is the essence of MongoDB, really. Doing things wrong made me frustrated because of the runtime errors.

Finally, the sections on replication and sharding were particularly problematic. The lectures kept mentioning that various defaults and APIs were a moving target. Furthermore, actual hardcoded numbers even showed up that we were supposed to use to specify policies. Wow.

## Conclusion

The lectures and quizzes were well-designed (until the final portion of the course that seemed particularly ad hoc). The assignments were sometimes rather tricky, with not much of a hint, but offered a decent variety of realistic query formation.

I would recommend this course as an overview to anyone who is committed to using MongoDB to its full extent.

However, although I got my "certificate" for completing the course, and appreciate that MongoDB, Inc offers this training for free, I felt that there was a lot of ad hoc stuff going on in the evolving design of MongoDB, and embedded in the API design as well. For what I needed to know for my work, I didn't really need the whole course.

