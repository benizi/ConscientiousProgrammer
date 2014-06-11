---
layout: post
title: "toString considered harmful, part 2"
date: 2013-12-26 22:03
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
- object-oriented
- string interpolation
---
This is part 2 of a series of articles, "`toString` considered harmful". [Part 1] introduced the problem in the context of a common design flaw present in object-oriented languages, and proposed a simple workaround.

In part 2, we look at advanced ways to organize "stringable" data, using either an object-oriented or functional style. Examples are in Scala because it equally supports either style.

<!--more-->

## Object-oriented vs. functional

The fix presented was in *object-oriented* style, adding a method `toUrlString` to a class. The other solution is the *functional* style, leaving the `Id` class alone, and writing an external function instead:

```scala
  case class Id(id: Int)

  def toUrlString(id: Id) = id.toString
```

with

```scala
    id match {
      case None => println("No id found!")
      case Some(n) => getUrl(makeUrl(toUrlString(n)))
    }
```

There are advantages and disadvantages to either solution.

## More advanced OO

It would be very natural, given a whole set of domain classes in addition to `Id`, to want all of them to have a `toUrlString`. Then the natural thing to do is to create a mini-universe (parallel to the `toString` universe) by creating a hierarchy:

```scala
  trait UrlString {
    def toUrlString: String
  }

  case class Id(id: Int) extends UrlString {
    override def toUrlString = id.toString
  }

  case class ...(...) extends UrlString {
    override def toUrlString = ...
  }
```

## Advanced string interpolation with OO

In fact, Scala and other languages with advanced string interpolation facilities allow yet another refactoring by making sure that what gets into a URL isn't just an arbitrary string in the first place!

Below we define a string interpolator that only operates on objects of classes that implement the trait `UrlString`, and therefore does away with an explicit call to `toUrlString`:

```scala
  implicit class UrlHelper(val sc: StringContext) extends AnyVal {
    def url(args: UrlString*): String = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buf = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buf append expressions.next.toUrlString
        buf append strings.next
      }
      buf.toString
    }
  }

  /** Only ever use UrlString to create a URL. */
  def makeUrl(id: UrlString): String = url"http://service.com?id=$id"
```

with

```scala
    id match {
      case None => println("No id found!")
      case Some(n) => getUrl(makeUrl(n))
    }
```

This may or may not be overengineering.

## Advanced string interpolation with FP

The functional approach doesn't like inheritance in the domain classes. We can implement it with [type classes](http://en.wikipedia.org/wiki/Type_class) (a concept first pioneered in Haskell in the late 1980s) by means of implicits in Scala, in order to implement `toUrlString` outside of a class hierarchy but also allow it to be used in a constrained generic way. A full discussion of this is outside the scope of this post, but the basic point is that with type classes, one can write code that does *not* depend on an inheritance hierarchy. If you're used to monkey-patching in dynamic languages, think of it as compile-time monkey-patching.

```scala
  // A type class
  trait UrlString[A] {
    def toUrlString(a: A): String
  }

  // Wrapper class
  case class Id(id: Int)

  // Implement the type class UrlString for Id
  implicit object IdToUrlString extends UrlString[Id] {
    override def toUrlString(a: Id) = a.id.toString
  }

  implicit class UrlHelper(val sc: StringContext) extends AnyVal {
    def url[A: UrlString](args: A*): String = {
      val strings = sc.parts.iterator
      val expressions = args.iterator
      val buf = new StringBuffer(strings.next)
      while (strings.hasNext) {
        buf append implicitly[UrlString[A]].toUrlString(expressions.next)
        buf append strings.next
      }
      buf.toString
    }
  }

  /** Anything "viewable" as UrlString can be used to create a URL. */
  def makeUrl[A: UrlString](id: A): String = url"http://service.com?id=$id"
```

## The final string gotcha

You may have noticed that there is still primitive obsession in this sample code: URLs are presented as `String` for simplicity. In real life, I use builders such as `URIBuilder` and `HttpGet` (Java [Apache HttpComponents](http://hc.apache.org/)) or more sophisticated Scala-specific libraries such as [Spray](http://spray.io/).

## Conclusion

I thought it would useful to compare an object-oriented and a functional approach to unifying data that share a domain-consistent notion of conversion to a string. Scala is a language that allows easy expression of both.

In part 3, we will look at languages that just don't have the `toString` problem at all.
