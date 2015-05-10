---
layout: post
title: "Pittsburgh Java User Group: Building and Evolving a Java API"
date: 2013-06-25T21:49:00
comments: true
categories: 
- PittJUG
- Pittsburgh
- Java
- C++
- Ruby
- ML
- Haskell
- Erlang
- null
- strings
- exceptions
- types
- Scala
- APIs
- error messages
- error handling
- test-driven development
- immutable
---
The topic for the featured presentation at the [Pittsburgh Java User Group](http://java.net/projects/pittjug/) (PittJUG) was "Building and Evolving a Java API", presented by Eric Stein of [Fulminatus Consulting](http://www.fulminatus.com/). I was excited to attend because API design is difficult but important, and today more than ever, because of the trend toward integrating many services to build an application, and toward providing such services (both externally and internally).

(For past reports of mine on PittJUG, see the [PittJUG archives in my other blog](http://franklinchen.com/blog/categories/pittjug/).)

The presentation was very good. (Eric's slides are available [here](http://www.fulminatus.com/presentations/PittJUG%20API%20Presentation.pdf), but as always with slides, be aware that it is just an outline, as good slides really should be, and the actual presentation and group discussion had all the substance.) Note that although some of the specific recommendations and examples are Java-specific, the entire area of good API design is *general* and is applicable no matter what programming language you are using.

However, the presentation also filled me with a combination of *sadness*, because our computing profession could have done better up front to make good API design much easier in various ways. On the positive side, it's better late than never.

A report on the presentation, with my commentary:

<!--more-->

## API design for whom?

Whenever doing something at all, the question "for whom?" should always be asked and answered. Eric observed that APIs must meet the different needs of different users, so it's useful to identify three specific end users with different needs, and look at the situation from each of their points of view:

- API owner
- specification owner
- logging owner

His inclusion of the "logging owner" was intriguing because the point of view from someone whose job it is to wade through logs is often slighted. But in light of ever more complex applications as well as errors and the need for recovery and quick turnaround in fixing them, I believe that Eric is right to argue that logging should be seriously designed, to "tell a consistent story". I am taking this advice to heart.

## Design process

Eric advised that design should start with use cases. And client code, from the perspective of the end user, as well as tests, should be written up front. He didn't use the term, but this is [test-driven development](http://en.wikipedia.org/wiki/Test-driven_development) (TDD), which in my mind has been an unfortunate term, because it is really *test-driven design*, which happens to have the nice effect of kick-starting development as well. (I'll be writing more later on this blog about TDD; I no longer start a new project without a TDD mentality and process, so productive and efficient I have found it.)

One thing that Eric said was to start with the *most important* cases first, not the easy cases. Very good advice: I have learned the hard way that it is easy to pat yourself on the back and start with easy cases to feel like you're making progress, but this often leads to hitting a block when getting to the hard cases, and then having to do some major redesign at that point. I've found that if something is really important, it needs to be addressed immediately, and often will drive the design of the components required to make the important hard case work. (I'll be writing more later on this blog about "the hard case", especially in the context of "sad paths" that are critical.)

## Stability

Stability is the big difficulty in API design. It's really daunting, actually, to think of the idea of "write once, support forever", but people get very unhappy, rightfully, if they start using your API and then things break on them after it changes.

## Simplicity

Eric argued that a good API should be easy to read as well as easy to write. This is all about human psychology.

Also, he noted that a side effect of his recommendation to write client code and tests first is that you get to see the point of view of the client, and can use this experience to weed out, for example, APIs that require a lot of annoying boilerplate code.

## Subclassing

There was a lot of good material covered about the dangers of exposing or mandating or allowing subclassing.

I agree with Eric about the *dangers of subclassing*. The ability to subclass existing classes is the single most abused feature of object-oriented languages, and when it comes to API design, the problems are even more magnified because of the needs for API security and evolution. Almost always, you want to use composition, not subclassing. I'm pretty annoyed that composition, which was well-understood in the 1960s and 1970s, ended up being "forgotten" in some circles. Eric gave the classic example of a totally disastrous API resulting from the Java library originally coming with a `Properties` class that was a subclass of `Hashtable`.

## Immutability

Immutable objects have huge advantages over mutable objects.

This is something has been known by the [functional programming](http://en.wikipedia.org/wiki/Functional_programming) community since Lisp in the 1950s and [ML](http://en.wikipedia.org/wiki/ML_(programming_language\)) in the 1970s and [Haskell](http://en.wikipedia.org/wiki/Haskell_(programming_language\)) and [Erlang](http://en.wikipedia.org/wiki/Erlang_(programming_language\)) in the 1980s.

But better late than never.

### My example: Apache HTTP Components

I have observed that APIs in such languages as Java and Ruby have more and more gravitated toward the use of immutable objects. A very good pattern I have seen is to use a builder (which has internal mutable state) to finally build an instance of an immutable object. Just recently, I was using the [Java Apache HTTP Components library](http://hc.apache.org/httpcomponents-client-ga/) and moved from the stable version (4.2.5) to the [beta version](http://hc.apache.org/httpcomponents-client-dev/) (4.3-beta2) (because of bugs in the stable version), and happened to notice that a whole slew of methods I was calling were deprecated in favor of a [new builder-based API](http://www.apache.org/dist/httpcomponents/httpclient/RELEASE_NOTES.txt). Excerpts from the release notes of the beta:

- Support for Java 7 try-with-resources
- Added fluent Builder classes
- Deprecation of preference and configuration API based on HttpParams interface in favor of constructor injection and plain configuration objects
- Reliance on object immutability instead of access synchronization for thread safety

It was very good to see that decades-old lessons about good design are finally making it into important Java libraries!

## Types

A lot of what Eric talked about had to do with types: type safety as well as the ramifications of types when evolving an API.

### `null`

Several of his examples (see his slides) involved the problem of `null`, [Hoare's billion-dollar mistake](http://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare), which I gave a little [talk about last year](http://franklinchen.com/blog/2012/09/06/my-pittsburgh-ruby-talk-nil/). Basically, `null` is a hole in a type system, and as a result, in Java one has to compensate by documenting in comments (but not having type checked) when and where something could be null or could not be, so that API clients know what to pass into methods and what to expect in return.

Java is not hopelessly behind: Java 8 has the [`Optional<T>` class](http://download.java.net/jdk8/docs/api/java/util/Optional.html) to help programmers transition away from using `null`. C++14 has a [proposal to add to the standard library `std::optional<T>`](http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2013/n3527.html).

Admittedly, these are late patches that do not actually prevent continued use of `null` (ideally, a programming language should simply not even have it; languages such as ML and Haskell from the 1970s and 1980s happily did without this terrible construct), but these are definitely steps forward. New languages being designed and implemented today from a clean slate have no excuse to perpetuate this billion-dollar error: for example, [Rust](http://www.rust-lang.org/) does not have `null`.

### Strings

Eric noted that use a string is throwing away type safety.

I think the use of strings everywhere is a disaster second to that of using `null`. The problem is that strings have to be parsed for information. And many people do not actually correctly do such parsing and validation, hence the prevalence of [injection attacks](http://en.wikipedia.org/wiki/SQL_injection).

Even apart from security, the manipulation of strings that actually are supposed to have an intended structure is a major cause of late-caught runtime errors, and of programmer confusion. For example, [this report on URLs](http://blog.lunatech.com/2009/02/03/what-every-web-developer-must-know-about-url-encoding) has been recirculating lately, because of errors in both libraries and client code when dealing with URLs as strings rather than as strongly typed data.

### Booleans

Eric observed that even booleans are an API design smell: booleans are a special case of an enumeration of possible states. What happens if you want to add a new state but your old API used booleans?

### Exceptions

Eric recommended *unchecked* over Java's [*checked* exceptions](http://en.wikipedia.org/wiki/Exception_handling#Checked_exceptions).

Sadly, in my Java programming in the past decade, I've had to deal with the fact that Java made a big mistake by introducing checked exceptions in the first place; the ramifications were not thought out then. Note that C++11 has finally deprecated exception specifiers, and exceptions in ML have always been unchecked since the introduction in the 1970s, while Java ignored two decades of experience and went ahead and bolted on checked exceptions.

#### Not strings!

Eric advised that all failure data should be included in a thrown exception. Too often, we see exception throwing code that just throws a string message. Then the catcher has to try to parse this message for important data (which may not even be present in the string). If you're going to throw an exception, write a class and stash away important information, not a string message.

## A short note on primitive obsession

My friend Adam who also attended the PittJUG meeting noted that what we're basically talking about is [primitive obsession](http://c2.com/cgi/wiki?PrimitiveObsession). The larger lesson here is that when designing robust, clear, and safe APIs, using primitives is often a mistake. Everything in the problem domain should really be modeled as its own type, not as a primitive.

## Evolution of an API

The trickiest thing to get right is making an API so that it can evolve in the future without breaking backwards *compatibility*. This compatibility can be of different forms:

- behavioral
- binary
- source

Eric talked a lot about weakening or strengthening contracts. There is a lot of hairy stuff involved, including knowledge of Java compiler internals. I think this is a space where more research is needed for the future in order to get the formal semantics of API evolution done right.

[japi-checker](https://code.google.com/p/japi-checker/) was mentioned as one tool that one can use for Java.

## Resources

In the context of software development in Java, it is impossible to avoid mentioning [Joshua Bloch](http://en.wikipedia.org/wiki/Joshua_Bloch)'s early book (subsequently revised for a second edition), "Effective Java". It is a daunting book, over 300 pages long, but it is an indispensable reference for anyone caring about good programming in Java. Eric called it the "gold standard".

Check out his links to other resources on his slides.

## Package design

A question came up about Eric's recommendation of using package-private visibility and his observation that a lot of people don't know about package-private, and don't design packages.

### Modules

My observation is that people don't design packages because they're not actually modules. *Java was invented without a module system*, although Java 8 originally was slated to finally include a module system in the form of Jigsaw, which was, however, [delayed to Java 9](http://www.infoworld.com/d/application-development/project-jigsaw-delayed-until-java-9-198007). Meanwhile, there is [OSGi](http://en.wikipedia.org/wiki/OSGi).

I don't like sounding like a broken record, but [Standard ML came with a module system in the 1980s](http://en.wikipedia.org/wiki/Standard_ML), and there has been further work in the area of module systems, including first-class module systems that go beyond stratified module systems. The reality is that Java was born in the 1990s an "old" language, with no positive technical innovations, that feels more like a language from the 1960s.

## Conclusion

Eric Stein gave an excellent talk about API design, both from the broadest perspective, focused on human beings and clients, and down and dirty with Java language features to avoid or use carefully.

## Addendum: my involvement with Java

I had a chance to review some of my feelings I have had about programming in Java myself. I've been a member of PittJUG for well over a decade now. I first signed up on the PittJUG mailing list in June 1999. I have not always been a regular attendee of the meetings: after some initial years of interest, I stopped attending for some years, and became regular again in 2009. So I've been around Java since it was a very young language, and all the while knowing of all the problems with the language. So why did I chose it as my main programming language a decade ago, and why have I stuck with it for so long, until last last year, when [I decided to write no *new* Java code, and move on to Scala](http://franklinchen.com/blog/2013/01/11/2013-is-my-year-of-scala/)? I will explain my decision-making in a forthcoming blog post.

## (Update of 2013-07-05)

An interesting [presentation on APIs in the enterprise](http://www.infoq.com/presentations/enterprise-api) came my way.
