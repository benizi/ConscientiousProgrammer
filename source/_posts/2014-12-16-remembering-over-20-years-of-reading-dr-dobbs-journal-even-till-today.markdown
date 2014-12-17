---
layout: post
title: "Remembering over 20 years of reading Dr. Dobb's Journal even till today"
date: 2014-12-16 22:23:42 -0500
comments: true
categories:
- Dr. Dobb's Journal
- Caml Light
- Haskell
- Gofer
- Dylan
- Scheme
- types
- gradual typing
---
It was with sadness, but not surprise, that I read today about the [end of Dr. Dobb's Journal](http://www.drdobbs.com/architecture-and-design/farewell-dr-dobbs/240169421).

I've been reading this magazine for *twenty-two years*! I haven't read any other magazine on any topic for this long.

<!--more-->

## RSS

Of course, it's been years since my last printed copy of [Dr. Dobb's Journal](http://en.wikipedia.org/wiki/Dr._Dobb%27s_Journal) (DDJ) entered my mailbox. At some point I was simply regularly reading content from its online [RSS feed](http://drdobbs.com/rss/all).

I was particularly sad when reading of the discontinuation of the publication because I had been following for quite some time Andrew Koenig's multi-part article on binary search. In fact, [I had just read part 9 and was looking forward to the eventual conclusion of the series](http://www.drdobbs.com/cpp/abstractions-for-binary-search-part-9-wh/240169416)! I've been waiting to see his punch line, since every article in this series has ended with a cliff-hanger. Here, he had closed with "Next week, we shall continue building our tests." I wonder if he be able to post the conclusion?

## How I started reading DDJ

In 1992, I was unemployed, a physics grad school dropout, and needed a new career. I had studied some math and passed the first two actuarial exams but frankly, had no interest in doing math and insurance for a living. Friends told me computer programming was a good way to go, so although I had not written a single computer program since high school (where I wrote and ran only COBOL and Pascal programs), I decided to learn C and Unix and Lisp, because these were technologies my friends had learned in their first year in college.

Meanwhile, this was all before the Web, so the way to get any new and exciting information was from Usenet or from printed magazines. I got some recommendations to check out two magazines in particular: Dr. Dobb's Journal and the [C Users Journal](http://en.wikipedia.org/wiki/C/C%2B%2B_Users_Journal).

Yes, I did ask, "Who the heck is or was Dr. Dobb?!" It didn't matter. The magazine was great. It covered all kinds of topics, and had code listings you could type in or download (remember [anonymous FTP](http://en.wikipedia.org/wiki/File_Transfer_Protocol#Anonymous_FTP) as the primary way to download stuff?).

{% img http://upload.wikimedia.org/wikipedia/commons/d/d8/Macintosh_classic.jpg Macintosh Classic %}

I got my first job as a software engineer in 1993, after much intense self-study, learning to programm in C on my younger sister's [Macintosh Classic](http://en.wikipedia.org/wiki/Macintosh_Classic) using [THINK C](http://en.wikipedia.org/wiki/THINK_C). What can I say, DDJ was there for me as a useful resource.

## Continuing to read DDJ; learning new languages

Today I was intrigued to read [Bodil Stokke's tweet](https://twitter.com/bodil/status/545009243272003585), "I read about both Haskell and Dylan in Dr Dobbs 1994-ish and was itching to learn both, but couldn't find Amiga impls for either."

I did learn both Haskell and Dylan in 1994, but don't remember whether it was because of mention in DDJ or because of other sources! I do remember that I downloaded quite a bit of "freeware" and "shareware" through FTP or bought through CD-ROMs in 1992-1994, before the birth of the [World Wide Web](http://en.wikipedia.org/wiki/World_Wide_Web), which I started using excitedly in 1995.

1994 was a particularly important year for me, because I discovered and experimented with writing and running programs in a huge variety of programming languages that year.

### Haskell

The most important new language I learned in 1994 was [Caml Light](http://caml.inria.fr/caml-light/), which I came across and learned and used on my Mac SE/30 in 1994, at [version 0.6](http://caml.inria.fr/pub/old_caml_site/caml-list-ar/0136.html). I still remember just finishing working through the [tutorial](http://caml.inria.fr/pub/docs/fpcl/) when version 0.7 came out and a lot of stuff changed, annoying me.  How Caml changed the course of my life is the subject of another article.

I also discovered Haskell in 1994. That took longer for me to get a feel for. It was the dialect [Gofer](http://en.wikipedia.org/wiki/Gofer_%28programming_language%29) that I first downloaded and copied to a floppy disk, in the form of [MacGofer](http://web.cecs.pdx.edu/~mpj/goferarc/macgofer/index.html). Later, in 1996, I used [GHC](https://www.haskell.org/ghc/) briefly for a small internal utility at work in 1996. I see that DDJ [mentioned Haskell in February 1996](http://www.drdobbs.com/programming-paradigms/184409831), but given that I learned it earlier, I don't know if there was any earlier DDJ mention (not Web-searchable currently) that could have influenced me to try Gofer in the first place. (More on the history of my Haskell usage is the subject of another article; Haskell is the only programming language I am still using active twenty years after first learning it.)

### Dylan

I see that there was a [DDJ article on Dylan in January 1994](http://www.drdobbs.com/tools/the-dylan-programming-language/184409404). "Dylan, an object-oriented dynamic language developed by Apple Computer, is designed to replace existing static languages for the development of large software systems, yet remains small and efficient enough for the next generation of portable computers. Dylan was developed from the language Scheme, augmented with the Common-Lisp Object System (CLOS)."

I was very excited about [Dylan](http://en.wikipedia.org/wiki/Dylan_%28programming_language%29) when I learned about it, because of its ambitions to simultaneously

- have all the nice features of Scheme
- remain dynamically typed, but offer optional typing as a standard part of the language
- provide an object system with multimethods (in 1994, I felt that conventional single-receiver OO was a dead end in the history of programming languages, as evidenced by the horrible [visitor pattern](http://en.wikipedia.org/wiki/Visitor_pattern))

Up until I discovered Caml, my favorite language was Scheme, which I had learned in 1992 while working through (the first edition of) ["The Structure and Interpretation of Computer Programs"](http://en.wikipedia.org/wiki/Structure_and_Interpretation_of_Computer_Programs) (SICP), so I felt totally at home in Dylan. In fact, I remember this very article as convincing me to switch from Scheme to Dylan: [Example 5](http://www.drdobbs.com/tools/the-dylan-programming-language/184409404#0272_00e9) in the article presented a code snippet from SICP translated into Dylan! Note that this was when Dylan was still using an S-expression syntax carried over from Scheme. Later, Dylan acquired an infix syntax, which I was a big fan of, actually, since I viscerally dislike the parentheses of Lisp languages. I played around with various implementations of Dylan before [Apple's project](http://en.wikipedia.org/wiki/Apple_Dylan) was [killed in 1995](http://web.archive.org/web/20060101181134/http://apple.computerhistory.org/discuss/msgReader$186?mode=day). That was a shocker. The news got to me late. I had acquired the Apple Dylan implementation and manual, and it was all for nothing. It was one of the greatest disappointments of my life (topic of another article).

## Conclusion

I've appreciated Dr. Dobb's Journal for over two decades because of the timely information it has brought on all kinds of topics involving software development, from C and assembly code listings to surveys of new languages, libraries, algorithms, etc. It was particularly exciting rediscovering an important article on Dylan that exposed me to a sadly short-lived language, Dylan (although some are trying to revive it as [Open Dylan](http://opendylan.org/)).

Some nostalgia and detective work have opened up memories and questions of "what could have been" that I'll explore in later articles.

**What is your relationship to Dr. Dobb's Journal? Have you ever been a loyal reader of it? What did you get from it?**
