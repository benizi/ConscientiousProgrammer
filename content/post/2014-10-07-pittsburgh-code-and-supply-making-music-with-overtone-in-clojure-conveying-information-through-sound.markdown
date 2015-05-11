---
layout: post
title: "Pittsburgh Code and Supply: Making music with Overtone in Clojure; Conveying information through sound"
disqus_identifier: "http://ConscientiousProgrammer.com/blog/2014/10/07/pittsburgh-code-and-supply-making-music-with-overtone-in-clojure-conveying-information-through-sound/"
disqus_url: "http://ConscientiousProgrammer.com/blog/2014/10/07/pittsburgh-code-and-supply-making-music-with-overtone-in-clojure-conveying-information-through-sound/"
date: 2014-10-07T21:10:18-04:00
comments: true
categories:
- Pittsburgh Code and Supply
- Pittsburgh
- music
- Clojure
- Overtone
- Emacs
- sonification
---
I attended [a fine meeting](http://www.meetup.com/Pittsburgh-Code-Supply/events/202086812) of [Pittsburgh Code and Supply](http://www.codeandsupply.co/) dedicated to two related topics: *music* and *sonification*. I thought it was a great idea to have presentations on both topics in the same session, thereby giving a broad view of what can be done with *sound* through computation.

<!--more-->

## Erik Swanson on Overtone in Clojure

[Overtone](http://overtone.github.io/) is a very interesting environment for programmable music written in Clojure that I'd heard of years ago but never gotten around to playing with, simply because my primary interest in music is [playing music on traditional physical instruments with my own hands, the old-fashioned way](http://franklinchen.com/blog/categories/music/). However, I am definitely open to the idea of doing interesting new things with computer aid; it just has not been a priority to explore.

As preparation for the presentation (I don't like feeling completely lost during live demos; I usually do!), I actually finally set up an Overtone project with Leiningen and walked through a tutorial that simply used the Clojure CIDER mode in Emacs to operate in a REPL. I didn't do any real live-coding, however. I just wanted to get a taste.

Erik Swanson gave a great presentation in which he described what he was doing as he live-coded some music with Emacs, incrementally creating instruments and generating pitches. Because of his presentation, I feel more comfortable about the prospect of really digging into Overtone if I ever want to.

Here's a video of his presentation:

<iframe width="560" height="315" src="//www.youtube.com/embed/w7ARayiKBrE" frameborder="0" allowfullscreen></iframe>

## [Keith Callenberg](https://twitter.com/keithcallenberg) on conveying information through sound

I learned a new word today: ["sonification"](http://en.wikipedia.org/wiki/Sonification), the use of non-speech audio to convey information.

I may have vaguely encountered sonification before, but never while fully attentive to it. That changed when Keith Callenberg, a computational scientist, gave a fine presentation in which he gave numerous striking examples of sonification. He made the important distinction between music and sonification: music is sound for an aesthetic purpose, but sonification is for information transfer.

He convincingly showed why sonification is useful for data analysis, and also noted that our culture is strangely dominated by the *visual*, so there remain many opportunities to use sound to understand data. (Also, sonification is used to help the visually impaired.)

One local example he gave was a particulate monitoring study done at CMU.

## David Souther on Web Audio in JavaScript

I thought we were done for the evening, but David Souther hopped up with an unscheduled lightning talk on using Web Audio in JavaScript.

## Conclusion

I enjoyed the presentations on music and sound, and got some ideas for stuff I might want to do myself. Another excellent session for Pittsburgh Code and Supply!
