---
categories:
- static site generator
- Octopress
- Hugo
- Jekyll
- Hakyll
- Ruby
- Go
- Haskell
- performance
- cache
- Pandoc
- Markdown
- Perl
comments: true
date: 2015-05-31T12:26:32-04:00
layout: post
title: "Why I switched from Octopress 2 to Hugo"
---

Until now, I haven't been publishing anything on any of my three blogs
for half a year now. There are many reasons, but one of them was that
I wanted to migrate away from
[Octopress](http://octopress.org/) 2. Octopress 2 is ancient and slow
and unmaintained, and I'd been waiting for
[Octopress 3](https://github.com/octopress/octopress) for over three
years
[to arrive](http://octopress.org/2015/01/15/octopress-3.0-is-coming/),
so when I heard that Octopress 3 was finally going to be officially
announced at [JekyllConf](http://jekyllconf.com/), I decided it was
time to migrate my blogs, to Octopress 3 or
[Jekyll](http://jekyllrb.com/), or something else entirely.

<!--more-->

## Factors to consider when choosing a static site generator

Some factors I kept in mind while evaluating a new static site
generator:

- Speed of full generation
- Speed of incremental generation
- Active progress in bug fixes, improvements, new features
- Availability of themes
- Community sharing, support
- Languages used for using and writing own templates and plugins

## Speed is critical for me

**Speed** was a huge consideration for me when I evaluated alternative
static site generators, so I was particularly interested in evaluating

- [Hugo](http://gohugo.io/) (written in Go)
- [Hakyll](http://jaspervdj.be/hakyll/) (written in Haskell)

The advantages of these two are that they are implemented in
statically typed languages that compile to native
executables.

Furthermore, Hakyll is a library, such that your configuration is
actually merely a Haskell program using the library, without the
indirection of configuration languages and interpreters of the
languages, and you can compile your site into a specialized native
executable. (For example, Hakyll uses Pandoc as a library for Markdown processing.)

## Advantages of going to Jekyll?

Jekyll is the most popular static site generator, so I had to evaluate
it despite knowing up front that it was not going to be a speed
winner. It might well be fast *enough*. The benefits of using a
platform with a large and passionate community are *tremendous*: bugs
get fixed, cool features get added, people step in to help you out if
you have questions, incremental improvements keep happening, themes
abound that you can just take and use. I never evaluate using a
technology based only on one consideration (such as speed).

Note that Octopress 3 is basically a really cool interface over an
underlying Jekyll setup, so I will only refer to Jekyll below, with
the understanding that all performance matters that apply to Jekyll
apply to Octopress 3 as well.

## Speed comparisons: Octopress 2, Jekyll, Hugo, Hakyll

### Full generation from scratch

My [personal blog](http://franklinchen.com/) has 585 posts. Here are
the from-scratch full generation times, based on migrations away from
Octopress 2 that I performed using a bunch of Perl scripts. Note that
the sites are not completely equivalent, because I only wanted to get
a rough idea, not compare total equivalence:

- Octopress 2: 5:39.28
- Jekyll: 15.90
- Hakyll: 14.51
- Hugo: 4.90

Jekyll and Hakyll don't do too badly, but Hugo was by far the fastest.

Although I suspect that as more features
get added to Hugo, it may slow down some, I also trust that since the
author and the Go community in general are *obsessed* with speed, Hugo
is a safe bet for anyone concerned about speed.

### Full generation but not from scratch

Hakyll stores a lot of information in a cache directory. If you've
done a full generation and change nothing and do a full generation
again (`my-compiled-site-builder build`), it comes back almost
instantaneously. If you've modified a file (as in the incremental,
server mode generation), my result was slightly slower than in server
mode:

- Hakyll: 2.23

### Incremental generation

I brought each generator up in "server" "watching" mode, to see what
would happen if I changed a single file, resulting in regeneration of
everything affected. For example, I changed the most recent blog post,
which affects its generation as well as potentially the main
page, RSS, sitemap, archive, tags and categories.

- Jekyll: 9.95
- Hugo: 4.11
- Hakyll: 1.50

It's interesting that Hugo's live "watch" functionality does not really
improve over regenerating the site from scratch.

For Hakyll, there is tremendous improvement. I believe this may be
because of the use of a cache directory but also because a
Hakyll-compiled generator incurs no *interpreter* overhead once you
have it running in server mode watching for changes.

9.95 seconds is still kind of slow for me, for making a quick change
to a file in progress and wanting to see how it displays in the
browser, so Jekyll is not optimal for me. But Hugo's 4.11 seconds is acceptable.

## So why not Hakyll?

So, given that Hakyll looks so promising, and I would far prefer
writing and debugging Haskell code, to hacking in some mixture of Go
templating and other configuration languages, why did I not migrate to
Hakyll?

There are many considerations that go into what I choose as a
technology to solve a specific problem. For example, there's a reason
I wrote all my one-shot little blog migration scripts in Perl, even
though I no longer write Perl for any other purpose (although Perl was
one of my main languages I used for nontrivial programs from
1999-2010).

Hakyll has a small community. I'm not sure I would even call it a
community. It's basically one guy's project. It is completely
unopinionated, such that to create any reasonable site you have to
write your own code or copy and paste from someone else's. There is no
formal concept of "theme" or an official theme sharing site.

Hakyll is pretty confusing to build if you don't use a Cabal sandbox,
and even then, there have perpetually been build problems of some kind
or another, for years. Last year, [I could not get it to build at all](https://github.com/jaspervdj/hakyll/issues/302).

Meanwhile, the [Travis build](https://travis-ci.org/jaspervdj/hakyll)
is perpetually broken and doesn't even test multiple versions of GHC
and Cabal. A call to
[get Hakyll into Stackage](https://github.com/jaspervdj/hakyll/issues/299)
is still open.

I ran into a
[serious YAML-handling bug that still has not been addressed after over a year](https://github.com/jaspervdj/hakyll/issues/225).

In other words, just because Hakyll seems to perform well on a
simplistic toy migration of my personal blog (after all the
workarounds for the bugs mentioned above) doesn't mean that I can
trust it to work if I do more complicated things, or that bug reports
will get addressed.

I'm writing this not to criticize the author of Hakyll, who by the way
writes a lot of quite high-quality
[blog posts on Haskell](http://jaspervdj.be/posts.html) and has a day
job developing in Haskell. Open source projects are labors of love
that just cannot be sustained by one person who has many things to do
in life.

## Winner: Hugo

It would be nice if there were a larger Hakyll community, but the
reality is that there isn't, and therefore as someone who also has
many things to do and prefers to write for my blogs rather than
implement features for the blog engine, I chose Hugo as the clear
winner for my current needs.

Hugo not only has an entire official
[discussion site](http://discuss.gohugo.io/) but also an active
[Gitter room](https://gitter.im/spf13/hugo).

The official documentation is pretty good and continues to be updated.

## Conclusion

Choosing a static site generator is like choosing any other software
to perform a task: you have to evaluate many different factors and
tradeoffs among the different choices available. For me, speed is very
important, but also a thriving, growing community of maintainers,
contributors, and users.

In the end, I chose Hugo, because it is fast, actively maintained, and
has a sizable community revolving around it.
