---
layout: post
title: "toString considered harmful, part 1"
date: 2013-12-23 23:54
comments: true
categories: 
- Scala
- Java
- Haskell
- Standard ML
- OCaml
- C
- C++
- C#
- Ruby
- Python
- Lisp
- Scheme
- Go
- Rust
- JavaScript
- object-oriented
- string interpolation
---
It is easy to rant about the problems or unexpected subtleties involving the use [strings](http://en.wikipedia.org/wiki/String_%28computer_science%29) in programming languages. This post, however, is not so much a rant about strings as about design and meaning, with `toString` only as an obvious example.

I'll describe a pitfall that came up in my code, and a solution, and make observations about how different programming languages address or avoid this problem.

This is part one of a series.

<!--more-->

## A bug when evolving my code

My example code is in Scala, but the problem illustrated actually extends to many (most?) other currently popular object-oriented languages as well, including Java (which Scala inherited `Object.toString` from), C# (`Object.ToString` stolen from Java), Ruby (`Object#to_s`), Python (`str`, which uses `object.__str__` in case of an object). (Later in the post, I discuss languages without this specific feature.)

### First working code

Here's the first version of the code, which is a simplification of logic in a real application. A name is looked up to find an ID, then the ID is used to construct a URL to submit to a Web service.

``` scala
object Example {
  type Id = Int

  /**
    @param name User name to look up
    @return ID of user
    */
  def findId(name: String): Id =
    if (name == "name") {
      42
    } else {
      0
    }

  def makeUrl(id: Id): String = s"http://service.com?id=$id"

  /** Simulate making the Web request. */
  def getUrl(url: String): Unit = println(url)

  def main(args: Array[String]): Unit = {
    val id = findId("name")
    getUrl(makeUrl(id))
    // output: http://service.com?id=42
  }
}
```

In this code, everything seems fine. This was the situation in my application when it was certain that finding an ID would succeed.

If you don't know Scala, just note that `s"...$id"` is just Scala's string interpolation syntax that behind the scenes calls `id.toString`.

### Non-working code

It turned out that finding an ID could fail, so I changed `findId` to return the type `Option[Id]` instead of `Id`. To get the code to compile, I had to change the type of the parameter to `makeUrl` also:

``` scala
  /**
    @param name User name to look up
    @return Some(ID of user) if found, else None
    */
  def findId(name: String): Option[Id] =
    if (name == "name") {
      Some(42)
    } else {
      None
    }

  // Oops, now this has an unintended bug!
  def makeUrl(id: Option[Id]): String = s"http://service.com?id=$id"
```

But this resulted in a bug (thankfully caught by my test suite that actually went over the Web to fetch stuff)! The bug was that the URL constructed was nothing resembling what I ever wanted to construct: `http://service.com?id=Some(42)` was being requested.

Furthermore, in the case of an ID not found, the URL constructed is `http://service.com?id=None`. How many of you have seen applications or Web sites or emails in which something was clearly missing and the text contained either an empty space or the string "null" or "nullvalue" such as

{% blockquote %}
Dear NULL,

You ordered NULL items.
{% endblockquote %}

Yup, you guessed it: someone wrote crappy code like what I just showed you, and frightening thing is, *it could have been me* and *it could have been you*.

#### What's the big deal?

You might think, "Big deal, you changed your code, ran your test, and immediately found the bug, so what's the problem?"

The problem is that I have higher standards than that. I don't want to rely on my tests to find my bugs. In fact, the test that went over the Web to do stuff was an *integration test*, not a *unit test*. The bug only manifested itself when the actual Web request failed. And as we see in real life, many apps are not sufficiently tested to root out all possible accidental string generations.

So although I caught the bug quickly, I caught it far less quickly than I wanted. I didn't want to even construct an obviously garbage URL like `http://service.com?id=Some(42)` at all. I prefer to have the type checker catch stupid design-level bugs up front. So I was furious at myself that I wrote code that the compiler was perfectly happy with but was obviously wrong. I had gotten lazy in more ways than one, and had been punished accordingly.

## A symptom of bad design

There were a couple of things wrong with my original code that made it not evolve well.

### Don't use `toString`

First, by using string interpolation at all, I was relying on the implicit `toString` method of all objects. String interpolation is an admittedly very convenient feature that I use extensively, but now I consider it rather dangerous.

But even if I hadn't used string interpolation, I would have had to build up strings myself anyway, and would have called `toString` *explicitly*, and I would have had the same problem: changing the type of something from `Id` to `Option[Id]` does not get rid of `toString`. In fact, in object-oriented languages where `toString` is defined way up at the top, *everything* has `toString`, whether you like it or not! The best you can do is override `toString`. (Actually, Scala "helpfully" generates a nice `toString` override for you when you use case classes, hence the output of `Some(42)`.) 

I consider this *global infection* a flaw in object-oriented languages that impose a set of methods on all objects whether you want them or not. `toString` is hardly the worst offending method, actually, but I'll save my complaints about others for later.

First step in cleaning up the code: make `toString` explicit:

``` scala
  /** Only ever use a String to create a URL. */
  def makeUrl(id: String): String = s"http://service.com?id=$id"

  def main(args: Array[String]): Unit = {
    val id = findId("name")
    getUrl(makeUrl(id.toString))
  }
```

(Later in the post, I will discuss alternatives to this explicit `toString`.)

### Primitive obsession

Another design smell was that of using

``` scala
type Id = Int
```

in the first place. This is a well-known lazy practice called [primitive obsession](http://c2.com/cgi/wiki?PrimitiveObsession). I know better than that.

The solution to primitive obsession is easy: create a new wrapper type. Hence, the original code, even before the possibly failing ID lookup, should have been

``` scala
  case class Id(id: Int)

  /**
    @param name User name to look up
    @return ID of user
    */
  def findId(name: String): Id =
    if (name == "name") {
      Id(42)
    } else {
      Id(0)
    }
```

Note that this still *would not* have solved the `toString` problem, since the output would simply have been `http://service.com?id=Some(Id(42))` or the dreaded `http://service.com?id=None`!

### `toString` is a problematic concept anyway

The real problem is one that transcends programming language design. (Later in this post I'll show languages that don't have `toString` but still easily allow a similar problem.)

The real problem is that strings are used for *multiple* purposes. Some are used just for debugging, showing an internal representation of data. Some are used for "human" reading. In fact, many languages distinguish between these two purposes: Lisp has [`write`, `prin1`, `print`, `pprint`](http://www.lispworks.com/documentation/HyperSpec/Body/f_wr_pr.htm); Scheme has [`write` and `display`](http://www.scheme.com/tspl3/io.html); Ruby has [`to_s` and `to_str`](http://ruby-doc.org/core-2.0.0/Object.html); Python has [`repr` and `str`](http://docs.python.org/2/library/functions.html).

One is often directed to override the "human-oriented" version of these mechanisms (implementing one's own special non-default format). In Java and Scala, that's `toString`. But this is precisely the problem. We are *encouraged* to abuse this built-in mechanism for getting a string from an object that is supposed to mean something in the *context of an application*. Yes, `Some(Id(42))` is a useful human-readable string, but it's not what I want to put into a URL for an ID parameter!

#### Different names for different contexts

Suppose you had a `Name` class, and it had fields such as `first` and `middle` and `last`. It's nonsensical to expect a single `toString` override to express all the different contexts in which you might want to get a single string from a full name. Sometimes you might want to generate `Franklin Chen`; other times, `Franklin Ming Chen`; other times, `Franklin M. Chen`; other times, `FMC`. The point is that there should really be a method for each of these. `toString` should be treated really as a debugging device.

Instead of piggybacking on `toString`, we should call a spade a spade, and define our own methods whose name is actually informative and tells us for what *purpose* we are asking for a string.

Let's refactor the code:


``` scala
  // Wrapper class
  case class Id(id: Int) {
    // Special method for turning to URL string fragment
    def toUrlString = id.toString
  }

  /**
    @param name User name to look up
    @return Some(ID of user) if found, else None
    */
  def findId(name: String): Option[Id] =
    if (name == "name") {
      Some(Id(42))
    } else {
      None
    }

  /** Only ever use a String to create a URL. */
  def makeUrl(id: String): String = s"http://service.com?id=$id"

  /** Simulate making the Web request. */
  def getUrl(url: String): Unit = println(url)

  def main(args: Array[String]): Unit = {
    val id = findId("name")
    // Will not compile because Option[Id] does not have toUrlString
    //getUrl(makeUrl(id.toUrlString))
  }
```

Now the code that was creating a junk URL will no longer compile: `id` is of type `Option[Id]` but that type does *not* have a `toUrlString` method. Mission accomplished!

To fix the code to get it compile, we handle both the case in which the ID is not found and the case in which it is:

``` scala
    // Will not compile because Option[Id] does not have toUrlString
    //getUrl(makeUrl(id.toUrlString))

    id match {
      case None => println("No id found!")
      case Some(n) => getUrl(makeUrl(n.toUrlString))
    }
```

Simple!

## The final string gotcha (to be discussed later)

You may have noticed that there is still primitive obsession in this sample code: URLs are presented as `String` for simplicity. In real life, I use builders such as `URIBuilder` and `HttpGet` (Java [Apache HttpComponents](http://hc.apache.org/)) or more sophisticated Scala-specific libraries.

However, at some point, data has to be turned into strings: this simply is how the Web works. It is at that point where one has to watch out. I will discuss that boundary in another post. String injection attacks are precisely a result of being sloppy about crossing that boundary.

## Conclusion

I gave a small taste of what the `toString` problem is about, and some initial steps toward solving it through better design even if the programming language encourages us to be sloppy.

In part 2 of this series, I will expand on different design choices even in the situation we just examined, especially in the face of continued evolution in which there are multiple domain classes to be turned into strings.

Finally, there actually are quite a few languages that don't have this particular `toString` problem, but some have analogues to a lesser degree. Part 3 of this series will discuss the different design choices in the languages or in the standard libraries or idioms. Examples will be drawn from C, C++, Haskell, Go, Standard ML, OCaml, Rust, and JavaScript.
