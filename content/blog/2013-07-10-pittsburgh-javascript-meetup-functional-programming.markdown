---
layout: post
title: "Pittsburgh JavaScript meetup: functional programming"
date: 2013-07-10T22:37:00
comments: true
categories: 
- Pittsburgh
- JavaScript
- functional programming
- functional language
- Pittsburgh TechFest
- immutable
- Java
- ML
- Standard ML
- OCaml
- F#
- Haskell
- higher-order
- first-class
- Scheme
- Ruby
- Python
- Perl
- Scala
- jQuery
- monads
- recursion
- parallelism
- functional reactive programming
- MVC
- .NET
- Rx
---
{% img http://photos3.meetupstatic.com/photos/event/7/f/6/e/global_239372622.jpeg Pittsburgh JavaScript %}

Because the topic of the featured presentation by [Richard Ashkettle](http://idiotcoder.com/) was [functional programming](http://en.wikipedia.org/wiki/Functional_programming), I made sure to attend my first meeting of the newly revived [Pittsburgh JavaScript meetup](http://www.meetup.com/Pittsburgh-JavaScript/). (This was actually the third meeting of the revived Pittsburgh JavaScript group, but I hadn't been able to make it to the first two.)

I'd met Richard earlier, a month ago at [Pittsburgh TechFest](http://franklinchen.com/blog/2013/06/01/report-on-the-second-pittsburgh-techfest-2013/). He does not claim to be an expert at functional programming, but is enthusiastic about concepts and techniques that he can and does apply to improving software quality in many dimensions. Since I have been a functional programming enthusiast and practitioner for twenty years, I had these goals in attending his presentations:

- evaluate what Richard and others have done with, and think is important about,  functional programming
- offer a few corrections, elaborations, suggestions as appropriate for the situation
- gather information on how I may be able to effectively explain functional programming to those who are new to it

<!--more-->

## History of the Pittsbugh JavaScript meetup group

Two years ago, in 2011, I had attended the [first incarnation of the group](http://www.meetup.com/Pittsburgh-JavaScript-Developers) faithfully, attending several months of meetings before it disbanded when the founder left Pittsburgh. I learned quite a bit from those meetings.

Two years is a long time in the world of JavaScript. Looking back at [one of my blog posts reporting on the old JavaScript meetup group](http://franklinchen.com/blog/2011/09/27/when-jquery-attacks/), I am amused by how anachronistic that feels to me today, as in my mention of [Sproutcore](http://sproutcore.com/), which I never hear about any more.

## Up front: my take on the secret of JavaScript

Here are some observations (not novel) I have about JavaScript, to set the stage for further discussion:

- JavaScript is full of horrible, disgusting Bad Parts.
- JavaScript has only one shiny Good Part: it has first-class functions.
- People manage to get a lot of amazingly cool and important stuff done using JavaScript, so take it seriously.

## Comments on Richard's presentation

### Immutability and "functional languages"

Richard talked about [immutability](http://en.wikipedia.org/wiki/Immutable_object) as being part of what "functional programming" is about.

Yes, it is, and in addition, common best practices in many programming languages these days argue for favoring immutability. I want to emphasize that you don't have to go full-blown into a specialized "functional language" in order to take advantage of immutability as desired: for example, this tip has long since been known as at least a "design pattern" in the [Java world](/blog/2013/06/25/pittsburgh-java-user-group-building-and-evolving-a-java-api/), in the [Ruby world](http://www.harukizaemon.com/blog/2010/03/01/functional-programming-in-object-oriented-languages/), and basically everywhere else.

Richard suggested that "functional languages" don't allow mutation. By my definition, this is *not* actually true.  I didn't raise an objection during the talk because I didn't want to sidetrack it, but here I have space to elaborate a little. I would argue (but that would have to be another blog post) that the cleanest, most intuitive and novice-accessible "functional languages") are those in the [ML](http://en.wikipedia.org/wiki/ML_%28programming_language%29) family originally developed in the 1970s and lives on today's popular, industrial-strength dialects of [Standard ML](http://en.wikipedia.org/wiki/Standard_ML), [OCaml](http://ocaml.org/), and [F#](http://fsharp.org/).

*ML fully supports mutation*, through [reference cells](http://en.wikibooks.org/wiki/Standard_ML_Programming/Types#References).

Personally, because of notions like this, I've been thinking that maybe it would be best if we all stopped using the term "functional language", because it has sadly become misleading and ambiguous. In particular, people instantly think "Haskell" when they use the word "functional language", when in fact Haskell is a very unusual, unique language (and ecosystem) among languages that one could call "functional".

### Expressions, functions, evaluation, and values

As Richard put it, "favor expressions evaluating to a value". The focus of functional programming is on *returning* a value, from an expression that includes function calls, rather than munging some mutable global state, or modifying state through a reference passed into a function as a parameter.

### Recursion vs. looping?

Richard noted that one characteristic of functional programming is the use of recursion instead of looping. He gave examples of writing tail-recursive functions.

One nitpick (which again, I did not bring up during the talk): recursion and looping (as in through "normal" constructs such as `while` and `for`) are *not* actually opposed. I will write a blog post later about why conventional looping is best thought of as a derived construct built on top of recursion, and why it might be best to consider conventional looping to be a historical accident and mistake (but a very useful invention given the historical circumstances).

#### Efficiency of recursion?

Also, people brought up the topic of efficiency. I have learned over the years that for many people, the word "recursion" seems to immediately trigger the word "efficiency". Richard did a great job in mentioning that [EMCAScript 6 is going to have proper tail calls (also called tail call optimization)](http://wiki.ecmascript.org/doku.php?id=harmony:proper_tail_calls), which will make tail recursion equivalent to ordinary looping in terms of space usage. This is *hugely important* news, of course. This mandate shows how serious JavaScript is about embracing functional programming.

#### Recursion for parallelism

But beyond the issue tail recursion, I felt I had to point out that recursive algorithms can offer actual *speedup*. The classic examples, of course, are the [parallel speedups of divide-and-conquer algorithms such as quicksort that use recursion](http://www.cs.cmu.edu/~scandal/nesl/alg-sequence.html#quicksort). If you expressed the algorithms by removing the recursion (with a hand-rolled stack as often taught in courses), you would destroy the inherent parallelism. So in the world of parallelism, you want your code to stay naturally recursive! As [parallel computing comes to JavaScript](http://adambom.github.io/parallel.js/), keep this in mind!

### Higher-order functions, first-class functions

Richard: "A function is an object".

It's really as simple as that. Functions can be created, stored, passed around. Functions can return other functions.

It's amazing, but I claim that this is JavaScript's one Good Part, its killer feature that surprisingly many languages don't have. And it's the one feature that is used *all the time*, not just in esoteric code. From the very beginning, JavaScript had first-class functions, to support callbacks for client code in Web browsers. and [Node](http://nodejs.org/) server-side code is nothing if not a whole bunch of `function (...)`. Without closures and first-class functions, JavaScript would have been completely useless. Thank goodness [Brendan Eich was inspired by the Scheme functional language](http://brendaneich.com/2008/04/popularity/).

#### No methods!

JavaScript not only provides full-fledged functions, but ironically, its weirdest feature, the prototype-based inheritance instead of class-based inheritance, inadvertently has prevented "interference" from the conventional class-based object-oriented world, which is mostly based not on functions, but on [methods](https://en.wikipedia.org/wiki/Method_%28computer_programming%29) that interference with functions because they are meant to *replace* functions (since according to "pure" object-oriented thinking, functions are evil and computation should involve method calls on objects). *JavaScript does not (really) have methods*; it only simulates them, by storing bona fide closures into fields of (map) objects. For example, if you use [Ember](http://emberjs.com), the following code illustrates how the building blocks of typical JavaScript programming involve creating lots of `function`s: `valueWillChange` and `valudDidChange` are just functions. They are not special things, "methods".

``` javascript
App.PersonView = Ember.View.extend({
  valueWillChange: function (obj, keyName, value) {
    this.changingFrom = value;
  }.observesBefore('content.value'),
  valueDidChange: function(obj, keyName, value) {
      // only run if updating a value already in the DOM
      if(this.get('state') === 'inDOM') {
          var color = value > this.changingFrom ? 'green' : 'red';
          // logic
      }
  }.observes('content.value')
});
```

### Examples

Richard gave some standard examples of using higher-order functions such as `map` and `filter` and `foreach` as provided in various popular JavaScript libraries such as [Underscore](http://underscorejs.org/).

As a matter of style, he recommended against long, obscure one-liners that involve chaining. I agree. For readability and testability of pipelines, I have found that it is good to break things up into intermediate steps. (Just [recently](/blog/2013/06/26/pittsburgh-python-night-of-the-favorite-module/), Tim Lesher also recommended against stupidity masking as excessive cleverness in his Python lightning talk about functional programming with `itertools`.)

### Monads?!

Regrettably, Richard brought up the term ["monads"](http://en.wikipedia.org/wiki/Monad_%28functional_programming%29). I'm close to thinking that this word should be [banned](http://fsharpforfunandprofit.com/posts/why-i-wont-be-writing-a-monad-tutorial/), because too many confusions about them have ruined the word. I have yet decided what the best alternative phrase might be. Maybe something like "computational context"?

Ironically, Richard admitted he was going to abuse the word, and for the sake of example, he said, he was going to say that "jQuery is a monad". I didn't want to derail his talk, and since he admitted the abuse, I decided there would be a better time to address this topic.

It was interesting to me, however, that he gathered that the interesting thing about jQuery, and why he wanted to call it a monad, was the chaining of calls in jQuery. I need to think of a good way to deal with this intuition when I get around to explaining monads.

### Partial application and currying

Richard explained both partial application and currying, then he wondered when one would in practice write a curried function.

I said (but not very convincingly, I think) that you would write a curried function if you knew you would otherwise have to manually do a bunch of partial application. I have to confess that was a weak answer. I think the practical situation in JavaScript is that full-scale up-front currying is not useful.

### Libraries

Richard recommended checking out the following libraries, especially Lo-Dash, which he judges to be faster than Underscore:

- [Underscore](http://underscorejs.org/)
- [Lo-Dash](http://lodash.com/)
- [Functional JavaScript](http://osteele.com/sources/javascript/functional/)

## Questions and answers

There were many interesting questions raised after the presentation.

### Introducing functional programming into a team environment

Someone asked Richard a good question about whether it's appropriate to, say, start using Underscore at work and expecting other team members to learn how to use it. Is it fair to expect others to learn a new way of doing things that might seem weird?

My point of view is that there's nothing specific about functional programming when it comes to questions like this. The same question comes up when evaluating a particular programming language or a particular MVC framework or a particular version control system, and the answer involve considering all the different realities of who is comfortable with what, whether it is worth learning something new, what the advantages and disadvantages are, how much the decision will affect delivery of business value.

### Growing parameter list for a function vs. passing in a big object

One concern brought up is that if you're programming in a functional style, you can end up being faced with ever-growing parameter lists for a function to pass in everything that it needs. One solution is to create a big object to stick everything into it that might be needed (for example, a configuration object instead of separate flag parameters). Is this overkill?

Also, [Tony Lukasavage](http://tonylukasavage.com/) emphasized that his concern was in the context of the need to evolve an existing API without breaking customers' client code that uses it.

My first note would be that API design is tricky and checking out [some resources on API design](/blog/2013/06/25/pittsburgh-java-user-group-building-and-evolving-a-java-api/) is very helpful.

Regarding parameter lists, Richard noted that you might have a design problem if you have a function that needs to know about so many things it depends on. You may want to refactor the function (of course, in the context of evolving a mature API that customers depend on, it may be too late, unfortunately) and the data structures passed in. For example, if you have a number of functions that all take as parameters a `firstName` and `lastName`, then maybe you want a single `name` parameter instead.

I suggested also that if certain flags are not always needed, then instead of a huge configuration object, how about simply having separate but related functions (in a language supporting overloading, you could provide overloaded functions, although the topic of overloading is another large one to discuss later), each of which takes only the flags the client cares about. Of course, as some people pointed out, then you might end up with annoyingly long and specific function names.

Nobody said designing an API is easy.

### Do I actually do functional programming?

One question I got, since before the meetup, I had indicated having experience with functional programming, was basically, do I actually use it in JavaScript and elsewhere. The answer is, yes. As I've mentioned, you don't have use a special language in order to program in this way, although it's much easier and more efficient to use a language that is tailored to support it. Most of the code I've written in the past fifteen years has attempted to be primarily functional in style, whatever language I'm using for work (mostly Java, Perl, Python, and recently Scala). I don't do this out of some kind of ideology, but because it works well in getting stuff done. Before adopting a primarily functional style of programming, I spent years working in traditional imperative and object-oriented styles, and empirically, that was less pleasant.

#### Do I do functional programming in JavaScript?

I have not actually done any JavaScript programming for work since doing some UI prototyping two years ago. I don't know when I'll return to doing front end work, but meanwhile, I am interested in doing some Web browser stuff for personal projects, so I try to keep up to date on developments in the JavaScript world.

One thing that has been interesting that I need to check out is the rise of [functional reactive programming (FRP)](http://en.wikipedia.org/wiki/Functional_reactive_programming) libraries for JavaScript, such as [Bacon.js](https://github.com/raimohanska/bacon.js). FRP was originally invented in the 1990s by Conal Elliott in the context of Haskell, but almost twenty years later, has finally spread well beyond that now to mainstream visibility and use. For example, Microsoft has thrown its weight behind [Reactive Extensions (Rx)](http://rx.codeplex.com/), which started out in the .NET world, but Microsoft then ported it to JavaScript, C/C++, Python, and Ruby.

## Conclusion

I was happy to see interest in functional programming in the local JavaScript community. Richard gave an introductory talk on it that I thought was very useful as a starting point for those who are new to it. I made note of some omissions or inaccuracies in order to improve my thinking about how I can contribute in the future to improving people's understanding and application of functional programming principles.
