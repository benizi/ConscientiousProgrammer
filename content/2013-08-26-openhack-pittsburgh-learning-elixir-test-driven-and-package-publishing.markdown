---
layout: post
title: "OpenHack Pittsburgh: learning Elixir test-driven and package-publishing"
date: 2013-08-26 22:09
comments: true
categories: 
- OpenHack
- Pittsburgh
- Erlang
- Elixir
- TDD
- testing
- concurrency
- asynchronous
- macros
- tau
- pi
- Travis
- continuous integration
- Haskell
- C++
- Perl
- Python
- Scala
- Java
- Clojure
- JavaScript
- Racket
---
It's been a long time since I went to an [OpenHack Pittsburgh](http://openhack.github.io/pittsburgh/) meeting. The last time was [five months ago](http://franklinchen.com/blog/2013/03/25/openhack-pittsburgh-exploring-scala-odds-and-ends/). On June 24, there was an [OpenHack Pittsburgh](http://www.meetup.com/pittsburgh-ruby/events/120200292/) held at [4moms](http://www.4moms.com/), but I really needed to stay home and take it easy that evening, because of my busy schedule later in the week, including attending a [PittJUG meeting](/blog/2013/06/25/pittsburgh-java-user-group-building-and-evolving-a-java-api/) and preparing a [Pittsburgh Python talk](/blog/2013/06/26/pittsburgh-python-night-of-the-favorite-module/).

[This August meeting of OpenHack Pittsburgh](http://www.meetup.com/pittsburgh-ruby/events/120201842/) was held at [ModCloth](http://www.modcloth.com/), which I had never been to. It's in Crafton, which is a place I've never been before, nearly half an hour drive from Pittsburgh. Because of worries about driving there near rush hour after work, I probably would not have signed up to attend had it not been for [Justin](http://justinxreese.com/), the organizer of OpenHack Pittsburgh.

What happened was that he asked for feedback about who was planning to work on what, and said he planned to learn some [Elixir](http://elixir-lang.org/), a fairly new programming language built on top of the [Erlang](http://www.erlang.org/) runtime. Since this language had been on my list of things to look into, I decided that learning loves company, so I proposed to learn some Elixir in a test-driven way!

What does that mean?

{% img http://distilleryimage10.ak.instagram.com/1e5597280ea711e3b2db22000aeb1b88_7.jpg Franklin working on laptop %}

<!--more-->

## How to learn a programming language

I have written code in probably around fifty (50) programming languages. I'm not kidding, and I'm not proud, but this is simply a fact of my having done programming since 1982, thirty-one (31!!) years ago. Let's face it: languages come and go. I have personally sat at an IBM [keypunch](http://en.wikipedia.org/wiki/Keypunch) machine punching out cards for my COBOL and Fortran code.

As with human languages, the fact is that to be truly productive in a language requires much more than learning some syntax and semantics. It requires actually writing and running and fixing code, and in the context of a realistic project.

I've learned through trial and error that the most common ways of learning a programming language, like the most common ways of learning human languages (other than one's first language), are terribly inefficient and misleading, resulting in minimal competence. I mean, reading through books, typing into a REPL, submitting a single file to a compiler or interpreter, etc.

I feel that we should learn programming languages using a variety of approaches. Yes, the academic bottom-up semantics-based approach is critical in the long run, to avoid wasting a lot of time on pointless misunderstandings. Exploration in a REPL can be very rewarding. But I think what is often missing is the *biggest possible picture*: immediately seeing how to write a program that is integrated with a test framework, uses libraries, is exportable and publishable publicly as a library, can be submitted to a continuous integration server, is editable in a decent editor or IDE, etc.

Without seeing the biggest possible picture, one usually gets a horribly inaccurate picture of what it is like to program in a particular language ecosystem. (I will be writing more about this subject of pedagogy later.)

## My proposal

The last couple of languages I have learned in recent years, I slowly came to work with in a more efficient way than in the past, because I focused on matters such as testability. In particular, as I grew to wholly adopt TDD when writing any code at all in any language, it became clear to me that the best way to learn a new language must be in a test-driven way.

Here's an analogy: I believe the best way to learn a human language is to dive into how to be able to converse in some coherent way right off the bat. [Tim Ferriss and others](http://www.fluentin3months.com/4-hour/) have exploited this idea to *drive* the learning of what is most relevant in [grammar](http://www.fourhourworkweek.com/blog/2007/11/07/how-to-learn-but-not-master-any-language-in-1-hour-plus-a-favor/) and [vocabulary](http://www.fourhourworkweek.com/blog/2009/01/20/learning-language/). 

In the past, I learned human languages in a terrible way, taking lots of courses, but never actually getting fluent in everyday use. This is how we tend to learn "foreign" programming languages. My proposal is that learning a new programming language should involve getting as quickly as possible up to speed on a *minimum viable publishable library*.

That the software artifact should be a *library*, and not just some executable program, is vital, because it means having to adhere by whatever standards apply to real-world API design in the language ecosystem, producing something that client code can import and call.

That it should be *publishable* is vital because it means adhering to standards of testing, of discovery through metadata, and enabling easy use by a client.

## My concrete example with learning Elixir tonight

### Prior background in Erlang and Elixir

I went to OpenHack Pittsburgh *never* having yet read any Elixir language material, never having written Elixir code, not even "hello, world". The only things I had done before tonight:

- I first heard of Erlang nearly two decades ago, and when I saw it, I was turned off by the Prolog-style syntax, and it didn't seem relevant to anything I was doing at the time, so I ignored it.
- I intended to learn some Erlang earlier this year, but made little progress before putting it aside: I went to the free book ["Learn You Some Erlang for Great Good!"](http://learnyousomeerlang.com/) and downloaded the code and [got it to compile](https://github.com/FranklinChen/learn-you-some-erlang), but did not proceed any further.
- I installed Elixir on my Mac [using Homebrew](http://elixir-lang.org/getting_started/1.html).
- I installed an [Emacs Elixir mode](https://github.com/elixir-lang/emacs-elixir).
- I made a note of a free online book ["Etudes for Elixir"](http://chimera.labs.oreilly.com/books/1234000001642) and checked out the GitHub repository, but did not look at the book or code.

### Find a testing framework

Upon arrival at ModCloth, getting pizza to eat, and plugging in my MacBook and connecting to the guest WiFi, the first thing I did was search for a testing framework for Elixir.

It's 2013, not 1993, so it is hard to convince me to invest any time at all in learning to operate in a language ecosystem that does not have at least a de facto standard testing framework. For every language that I currently actively use for writing serious programs, I operate with TDD. Some examples of languages and the test frameworks I currently use for them:

- C/C++: [googletest](https://code.google.com/p/googletest/)
- Clojure: [Midje](https://github.com/marick/Midje)
- Haskell: [HSpec](http://hspec.github.io/) and [QuickCheck](http://www.haskell.org/haskellwiki/Introduction_to_QuickCheck2)
- Java or Scala: [Specs2](http://specs2.org/) and [ScalaCheck](http://www.scalacheck.org/)
- JavaScript: [Jasmine](http://pivotal.github.io/jasmine/)
- Perl: [Test::More](http://search.cpan.org/~mschwern/Test-Simple-0.98/lib/Test/More.pm)
- Python: [pytest](http://pytest.org/)
- Ruby: [RSpec](http://rspec.info/)
- Racket: [RackUnit](http://docs.racket-lang.org/rackunit/)

Well, first Web search hit for Elixir and unit testing was [ExUnit](http://elixir-lang.org/getting_started/ex_unit/1.html), so that was encouraging.

Justin was still stuck installing Elixir, so we started exploring Elixir together off my laptop.

### One test file

The first thing I did after discovering ExUnit was to copy and paste the code for a sample test into Emacs and save it into a file `test_hello.exs`, see that the Elixir mode was working (with syntax highlighting), and figure out how to run the test: `$ elixir test_hello.exs`.

It worked!

Then we explored adding more tests, changing some assertions, and learning as went, by looking up the Elixir documentation. For example, Elixir uses an interesting callback mechanism for cleanly passing "setup" state to tests inside a single test case. Also, `assert` and `test` are [syntactic macros](http://en.wikipedia.org/wiki/Macro_%28computer_science%29#Syntactic_macros), enabling them to do magic to analyze expressions, hence the support of natural assertion syntax (which [I like a lot better than special syntax](/blog/2013/07/11/pittsburgh-ruby-lightning-talk-night/)).

After writing some failing tests involving arithmetic and making them pass, I wrote a division by zero test. I was intrigued that not only did it fail, as expected, but also the compiler warned ahead of time that an exception was going to be raised. Nice. In any case, I learned how to make an assertion of the raising of an exception. This is very important. We should always pay attention to [testing the sad paths](http://franklinchen.com/blog/2013/06/01/report-on-the-second-pittsburgh-techfest-2013/).

We saw in the documentation for ExUnit that there was a way to mark a test case as being OK to run concurrently with others. Great! These days there is no excuse not to be able to run tests concurrently. So I wrote another test case, which I put in the same source file (for the moment), and I wanted to see for myself that concurrency was happening. I wanted to call some kind of "sleep" function. To do that, I did a Web search, and found that Erlang comes with `sleep` in the `timer` module. I found out how to call Erlang from Elixir, and it worked!

### More than one source file

The next obvious step was to write a test for code that did not yet exist, but was to be written outside the test source file. For that, I had to figure out how to define a module and import it. OK, but then there's the question of file organization. Hmm.

At this point, the only reasonable thing to do (and which in retrospect should have been done first) was to find out what the "standard" way of organizing a project's source files is supposed to be, and what the standard way to build everything is.

A Web search quickly came up with [Mix](http://elixir-lang.org/getting_started/mix/1.html). Awesome, the Elixir people are serious about real software development! Mix turns out to be inspired by Clojure's [Leiningen](https://github.com/technomancy/leiningen), which of course I use to start any new Clojure project. `$ mix new hello` did the trick, creating the scaffolding for a new Elixir project.

I refactored our existing `test_hello.exs` file into two files (one for each test case) in the `test` directory, then wrote a module `hello.ex` into `lib`. A failed test, then making it succeed, and finally I wrote my first non-test code in Elixir!

Running tests is very easy once you are using Mix. Just `$ mix test` does it.

### Packaging for publshing

At this point, I decided I wanted to create an actual Elixir package to publish.

So I did a Web search to find out whether there was a standard repository and how to publish to it.

Here we go, [Expm](http://expm.co/)!

I installed the package manager, set up my user name and password, and went to work on a simple package to publish.

I saw that nobody had yet contributed an implementation of [tau](http://franklinchen.com/blog/2012/03/14/for-real-geeks-today-is-not-pi-day-but-half-tau-day/), so I quickly created a tau library with Mix, did `$ expm new` to create a `package.exs` template, filled it out, wrote an ExUnit test that passed, completed the `README.md`, and just as OpenHack Pittsburgh was officially closing for the evening, I successfully published [my tau package](http://expm.co/tau)!

Actually, I accidentally screwed up the first time, forgetting to change all the fields of the template `package.exs` and ending up published `yourlib`.

Oh, and I committed my project in Git and pushed [to GitHub](https://github.com/franklinchen/tau).

## Some Haskell stuff

Steve McCarthy of [Spacefinity](http://www.spacefinity.com/) had announced, in the introduction at the beginning of the evening when each of us stood up to say what we planned to work on, that he wanted to do more with the [Cabal](http://www.haskell.org/cabal/) package manager for Haskell, so I spent a little bit of time getting him going by pointing him toward a sample Haskell code repository I initially [set up last year when some friends were interested in getting into Haskell](http://franklinchen.com/blog/2012/11/05/the-start-of-a-local-haskell-study-group.markdown/); sadly, we've all been too busy for Haskell, so I've let the project rot, in the sense that my configuration file is not optimally organized and also there have been updates in the test framework world in Haskell since then.

Anyway, here's my ["Project Euler in Haskell" repository](https://github.com/FranklinChen/project-euler-haskell). I plan to get around to improving it.

## Thanks

Thanks to ModCloth for providing the space, pizza, cookies, and WiFi!

A big thank-you to Justin for providing the inspiration for me to try this experiment in learning!

And thanks to Steve McCarthy for getting me to drag up an old Haskell project and making me think that I should soon update it to be a better example.

## Conclusion

Tonight was the first time I ever started learning a new programming language, wrote unit tests right off the bat with proper project organization and also published a library immediately.

Of course, there is much more to go in actually really learning and using Elixir, but as a first step in showing that Elixir has a viable ecosystem, I think the experiment was a success. My next steps in continuing to learn Elixir would include writing tests in conjunction with a deep dive into the actual semantics of the language. There is no substitute for the latter; the alternative is dangerously making assumptions based on superficial syntactic similarity with other languages (such as Ruby).

### (Update of 2013-08-27)

One thing I didn't get around to was setting up [Travis](http://travis-ci.org/) continuous integration. I believe this should also be part of the story in getting up and running in a new programming language environment.

So I did a Web search and figured out how to create a suitable `.travis.yml`. I committed and pushed to GitHub, flipped the switch in my Travis account, and you can see its [passing status](https://travis-ci.org/FranklinChen/tau).
