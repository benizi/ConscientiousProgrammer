---
layout: post
title: "Pittsburgh Python: night of the favorite module"
date: 2013-06-26T22:50:00
comments: true
categories: 
- Pittsburgh
- Python
- IPython Notebook
- Scala
- Sentry
- logging
- command line
- parsing
- domain-specific languages
- convention
- testing
- property-based testing
- QuickCheck
- ScalaCheck
- functional programming
- iterators
- lazy sequences
- community
---
{% img http://photos4.meetupstatic.com/photos/event/d/e/b/e/global_187797022.jpeg Pittsburgh Python User Group %}

The [Pittsburgh Python User Group](http://www.meetup.com/pghpython/) had [another "favorite module night"](http://www.meetup.com/pghpython/events/120442102). I enjoy this format because I get to learn about what people find useful, so that I may perhaps use it myself!

## How I benefited in the past

For example, [one of these "favorite module night" sessions](http://franklinchen.com/blog/2012/08/23/pittsburgh-python-meetup-i-gave-my-first-lightning-talk-ever-the-topic-was-scons/) was where I learned about Kenneth Reitz's `requests` library. Very often it is easy to not be aware of the rapidly changing ecology of very useful libraries in a programming language ecosystem outside of the official "standard library", especially when using a language that is *not* one's primary working language. For example, Python has never been one of my primary working languages, so I'm not as up to date on what all the best tools are to use. But just a couple of weeks ago, I had to do some work with Python, and `requests` came in very handy.

## [IPython Notebook](http://ipython.org/notebook.html)

Josh Adelman opened the show-and-tell with a demo of [IPython Notebook](http://ipython.org/notebook.html). This is a truly fantastic interactive environment for developing code while creating a full document for publication and sharing. He uses it not only for research but also for his students to use in his teaching.

If you've used Mathematica, the concept is similar to that environment.

I'm also excited that IPython Notebook has been the direct inspiration for similar projects for other languages, e.g., [Scala Notebook](https://github.com/Bridgewater/scala-notebook) for Scala in development.

Josh gave a link to this [cool blog that uses IPython Notebook](http://jakevdp.github.io/).

## [Sentry](https://getsentry.com/welcome/) and [Raven](http://raven.readthedocs.org/en/latest/)

Nick Sloan presented on Sentry, a service for collecting errors in your program and notifying you of them, and Raven, the official Python client for Sentry. Sentry is actually open source, so you could host it yourself, but paying for the hosted service provides conveniences.

Using Sentry is no more than using ordinary Python logging after calling a setup function. The ease of use made it sound like a winner.

## [docopt](http://docopt.org/)

Joe Esposito presented on `docopt`, a library for command line parsing. The interesting thing about it is that it implements an *external* domain-specific language for describing command lines: you write a usage message, as a string, and `docopt` parses it in order to deduce what the expectations and constraints are. This is a very ambitious approach, contrary to the usual internal API-based approaches, some of which are sophisticated and implement an *internal* domain-specific language.

Of course, Joe contrasted this library with one of the standard command line parsing libraries for Python, `argparse`, which is lower-level so that when you use it, the high-level end user usage is not as immediately readable.

There were a lot of good questions about `docopt` since it seemed magical. I've decided to write more about all this in a separate blog post about command line parsers.

## [hypothesis](https://pypi.python.org/pypi/hypothesis)

I gave a short 5-minute presentation on `hypothesis`, a library in development for doing property-based testing, inspired by QuickCheck and ScalaCheck. As I have done recently when giving a much longer [talk on property-based testing using ScalaCheck](http://franklinchen.com/blog/2013/04/11/my-pittsburgh-scala-meetup-talk-on-property-based-testing-using-scalacheck/), I focused on motivating the use of property-based testing as an addition to example-based testing.

Materials for my lightning talk are [here](https://github.com/franklinchen/lightning-talk-on-hypothesis), and my few slides are below:

<iframe src="http://www.slideshare.net/slideshow/embed_code/23548866" width="427" height="356" frameborder="0" marginwidth="0" marginheight="0" scrolling="no" style="border:1px solid #CCC;border-width:1px 1px 0;margin-bottom:5px" allowfullscreen webkitallowfullscreen mozallowfullscreen> </iframe> <div style="margin-bottom:5px"> <strong> <a href="http://www.slideshare.net/FranklinChen/handout-23548866" title="5-minute intro to property-based testing in Python with hypothesis" target="_blank">5-minute intro to property-based testing in Python with hypothesis</a> </strong> from <strong><a href="http://www.slideshare.net/FranklinChen" target="_blank">Franklin Chen</a></strong> </div>

## [`itertools`](http://docs.python.org/2/library/itertools.html)

Tim Lesher talked about `itertools`, a very useful part of the standard library that provides support for efficient functional programming idioms. Part of the efficiency comes from the use of iterators in order to avoid constructing intermediate lists; this is a way of simulating the lazy sequences that are standard in languages such as ML, Haskell, Scala, and Clojure.

Tim noted that although `itertools` is powerful, "don't be stupid": don't write obscure-looking code with it just because you can.

Josh noted that the documentation for `itertools` is great, with recipes that show you not only how to do things, and also with code for the equivalent more complicated code that you would have to write if you didn't use `itertools`. I agree that the documentation for `itertools` is a model of high-quality documentation for a library. Check it out, and use it!

## [`argparse`](http://docs.python.org/dev/library/argparse.html)

Craig gave a little presentation on `argparse`, the aforementioned standard library for command line parsing. He noted it was ironic that he had prepared to speak on it before Joe independently decided to talk about `docopt`. (Actually, I like `argparse` and had considered talking about it before Craig submitted it already.)

One feature he uses from `argparse` is the ability to create mutually exclusive groups. There was a question of whether this is supported in `docopt`.

As mentioned earlier, I'll write more about both `argparse` and `docopt` in a separate blog post.

## [The Python Standard Library by Example](http://doughellmann.com/python-standard-library-by-example)

Someone shared not a "favorite module", but a favorite book, "The Python Standard Library by Example". I'm happy he did, because it is in fact an excellent reference for anyone using the Python standard library, with concrete examples that you can take and use. Too often, I have found that standard documentation is too terse.

The book is based on ["Python Module of the Week"](http://pymotw.com/2/), which is a great online resource.

## Introductions for new people

An important part of the vibe of the Pittsburgh Python User Group is how it tries to get everyone actively involved in the local Python community.

There is a tradition of periodically having everyone introduce themselves, to counter the tendency sometimes in these kinds of groups for people to attend a meeting out of curiosity and then silently leave and perhaps not come back, out of intimidation or not knowing how to benefit or contribute. I know I have done that before, especially when attending the meeting of a group where I didn't really know anybody ahead of time and still knew very little about the topics discussed by the group (such as when I joined the Pittsburgh Ruby group as one who had barely used any Ruby).

Since so many people showed up, and a lot seemed to be new faces, we had a round of introductions. I hope we'll continue to see some of these new faces in the future!

## Conclusion

Thanks again to Google Pittsburgh for hosting the Pittsburgh Python User Group meeting, and Steve Gross of Google for being the guy who makes it able for us to meet there!
