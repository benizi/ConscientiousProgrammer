---
layout: post
title: "Introducing this new programming blog"
date: 2013-06-24T09:40:00
comments: true
categories: 
- bikeshedding
- blog software
- Octopress
- Jekyll
- Ruby
- Hakyll
- Haskell
---
Hi. My name is Franklin Chen. I already have a [personal Web site and blog](http://franklinchen.com/).

But I ended up only writing [a little bit about programming on that blog](http://franklinchen.com/blog/categories/programming/).

So I decided it was time to start a separate blog to devoted entirely to computer programming. The problem with the all-in-one personal blog was that I ended up never really working up a momentum to post the kinds of articles I wanted to write about programming.

## Why "The Conscientious Programmer"?

At some point in the past year or two, I reflected on how I could best summarize my attitudes and aspirations as a programmer, as someone who loves programming and also works as a professional software developer.

I came up with the decidedly non-sexy term *conscientious programmer*. It is also a term that requires explanation, because you could easily interpret it as having a connotation that is moralistic, pretentious, or boring!

It is impossible to summarize in a sound bite what I mean by the term, so I have created this blog in order to continually illustrate, by example, the kinds of technical and other issues that come up in software development that I grapple with.

However, since a sound bite is required in some circumstances, here's what I currently have:

{% blockquote %}
I am a conscientious computer programmer, committed to designing, building, and testing correct, efficient, documented, maintainable software that meets users' real needs.
{% endblockquote %}

My material will come from my own experiences (successes and failures, past and present) as well as from those of others. See the [About page](/about/) for more sound bites about what this blog will be about.

Let's start immediately with my personal experience from just the past couple of days!

## Future topics

But first, here is a little sample of topics I will write about in the future:

- positive and negative experiences with statically typed languages
- positive and negative experiences with dynamically typed languages
- the good and bad of the polyglot programming world
- what is object-oriented programming anyway?
- what is functional programming anyway?
- Agile notions
- test-driven development, behavior-driven development, other testing topics
- static analysis
- code performance
- human performance
- controversies over the adjective "pragmatic"
- how much theory should one know or use?
- how to teach
- how to learn
- how to focus and not be a dilettante
- how to spread ideas
- is syntax important?
- are macros a good idea?
- are unsound type systems justified?
- what is a type system anyway or what should it be?
- monads
- editors and IDEs
- are software patterns a mistake in disguise?
- software evolution and maintenance
- planning for the future vs. getting it done today
- role of emotion in decision making
- what is "community"?
- politics of open source vs. free software
- open standards
- questions of diversity in our profession
- education

## The dilemmas I encountered when deciding to create this blog

I already encountered a dilemma some years ago when I thought that I should have a personal blog to regularly post to. I had messed around with Blogger and WordPress and they were not at all suitable for me. Luckily, I discovered [Octopress](http://octopress.org/) and it got me going very quickly. I'm still using it.

If all were going smoothly with the personal blog, then I probably would not have any dilemma when deciding to create the new blog; I would just use exactly the same Octopress platform.

*Or would I?*

### Desire for novelty: good or bad?

I am not immune to the thrill of novelty. I am not an extreme [early adopter](http://en.wikipedia.org/wiki/Early_adopter), but I also do not entirely subscribe to the philosophy ["if it ain't broke, don't fix it"](http://en.wiktionary.org/wiki/if_it_ain%27t_broke,_don%27t_fix_it). I like opportunities for doing something differently and better than in the past, but I like to evaluate these rationally. This is an example of what I mean by "conscientious programmer": *always be aware of risks and tradeoffs when making any kind of decision*.

One thing that is sometimes forgotten in the rush to make a decision is that it is premature to immediately start making lists of risks and tradeoffs. First, you have to step back and figure out what the real goals are. Many projects fail because the assessments and the followup actions were *correct*, but toward the *wrong* goal. *Solving the correct problem* is more important than *solving the wrong problem correctly*.

In the case of choosing which software to use for my new blog, there were actually several considerations.

### Unstable branch of Octopress

Octopress's stable [`master` branch](https://github.com/imathis/octopress) is a continuation of version 2.0. At some point for my personal blog, I got excited about trying out the [unstable `2.1` branch](https://github.com/imathis/octopress/tree/2.1) because of various improvements. I migrated over, and periodically endured a surprising amount of pain. It wasn't just because I had to manually move files around and resolve merge conflicts and refactor my config files, but also bugs kept popping up, and I had to spend time fixing them.

In retrospect, *it was a mistake being an early adopter* of branch `2.1`. I stopped pulling and merging over a month ago, when it became clear that the migration to [Jekyll](http://jekyllrb.com/) 1.0 was causing major refactorings by the developers of the Octopress `2.1` branch that were breaking everything on my blog.

It would *not* have been a mistake if my goal were to help test and develop Octopress. That is a noble goal, because Octopress has been a *huge* positive contributor to the world of blogging, as a static site generator that is programmable and comes with a good default theme (which I still use). But my *real goal* is to write and post content, not spend my time fixing or configuring Octopress.

### Coming back to Octopress `master` branch?

The official recommendation of the Octopress team right now ([as of two days ago, June 22](https://twitter.com/octopress/status/348465809624027136)) is to use `master`, because once `2.1` is cleaned up, it will actually become `3.0` and there will be a tested migration path from `2.0`.

I confess to having a psychological resistance to "downgrading", for this new blog, from my `2.1`-based setup on my personal blog, but this is exactly the moment when it is necessary to remember that I want to be a *conscientious* programmer.

### Switching to a new blogging platform?

The idea also occurred to me to use a completely different blogging platform. [Hakyll](http://jaspervdj.be/hakyll/) has always seemed appealing to me, because it is driven by Haskell (Octopress is driven by Ruby). I prefer programming in Haskell to programming in Ruby, both because of the static type safety and because of the expected considerable performance improvement in site generation (right now, full generation of my personal blog takes a very long time).

But after looking at Hakyll, and being tempted to get into it, I decided that my real goal is *not* to spend a lot of time hacking Haskell as a personal side project, and creating cool themes and features for Hakyll, but to get a blog up and running so that I can focus on content for the blog. [Bikeshedding](http://en.wiktionary.org/wiki/bikeshedding) is not what I'm here for.

So I used Octopress again for this new blog.

By the way, if you are interested in using Octopress, here is a great [tutorial](http://webdesign.tutsplus.com/tutorials/applications/getting-started-with-octopress/).

### Flexibility

Another thing I try to remember is to avoid getting backed into a corner when it comes to decision-making. I like to think about the future as well as the present. *What if...?*

I know that if I wanted to, in the future I could switch blogging platforms at will, by doing some programming. The decisions that I am making *now* are not irreversible. Using a programmable static site generator means that I have full control over paths, styles, generation of RSS feeds, insertion of JavaScript-based features (such as the [Disqus](http://disqus.com/) commenting system), etc.

Thinking up front about the future and how it might be different and what I might need or want to do in the future is not just idle daydreaming; I find it an essential final step in making a decision *now* and focusing on action *now*, with the peace of mind that I no longer have to worry about the future because I already have a [Plan B](http://en.wikipedia.org/wiki/Plan_B). We must always assume possible failure and future change. But we don't have to panic about it.

## Conclusion

I've started this new programming blog, using the stable `master` branch of Octopress. I hope we'll share some interesting experiences here!
