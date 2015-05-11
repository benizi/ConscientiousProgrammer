---
layout: post
title: "Pittsburgh Ruby/Python social and some observations about our polyglot world"
date: 2013-08-02T22:17:42-04:00
url: "blog/2013/08/02/pittsburgh-ruby-python-social/"
comments: true
categories: 
- Ruby
- Python
- Perl
- NLP
- C++
- Scala
- MongoDB
- futures
- parallelism
- concurrency
- Bakery Square
- Farsi
- METAL project
- Carnegie Mellon University
- Pittsburgh
---
Programming meetups aren't just about presentations and coding. Abby and I joined others in meeting up for dinner for a [joint Pittsburgh Ruby and Pittsburgh Python social](http://www.meetup.com/pittsburgh-ruby/events/120201652/) and enjoyed relaxing and socializing outdoors in Bakery Square. Although it perpetually looked like it was going to rain, it turned out we weren't really rained on.

<iframe src="//instagram.com/p/ch8quus4Kb/embed/" width="612" height="710" frameborder="0" scrolling="no" allowtransparency="true"></iframe>

Carol, Andre, and Abby:

<a href="https://www.flickr.com/photos/carolnichols/9425969330" title="ttm! by Carol Nichols, on Flickr"><img src="https://farm4.staticflickr.com/3754/9425969330_d8ecdbf987.jpg" width="500" height="281" alt="ttm!"></a>

It was fitting that there was a joint social for two language communities, because we live in a polyglot world.

Despite my original intention not to engage into any tech-related conversation, I couldn't help remarking on my current polyglot responsibilities at work at CMU on the METAL project!

<!--more-->

## Our polyglot world: Ruby, Python, Perl

I mentioned having to cobble together Ruby, Python, and Perl code recently in an NLP project to process Farsi text into corpora to analyze. We used a Ruby gem [Virastar](https://github.com/aziz/virastar) in our pipeline, a Perl script for a Farsi stemmer, [Perstem](http://perstem.sourceforge.net/), and a [Python script for normalization](https://github.com/wfeely/farsiNLPTools).

### (Update of 2013-12-04) C++, Scala, MongoDB

The work project ended up becoming even more polyglot than I expected.

I also used a C++ tagger, TurboTagger from [TurboParser](http://www.ark.cs.cmu.edu/TurboParser/).

I wrote Scala to execute and monitor the pipeline, because it turned out that component bugs and timeouts (from infinite loops sometimes) had to be dealt with: the original shell script pipelines did not handle any of that at all. Also, the vast amount of data meant that parallelizing was critical. The Scala ecosystem has  turned out great for me. I didn't use Akka actors for this task, just futures and parallel collections.

I also used MongoDB because it is quite a natural fit for document-oriented storage and querying as an intermediate stage I can inspect before the final stage of conversion to Sketch Engine format. And I happened to just [finish a free online MongoDB course](/blog/2013/12/04/mongodb-free-online-course-a-review/) which has been useful. In my Scala code, I used the official Scala driver [Casbah](https://github.com/mongodb/casbah).

Why MongoDB? I needed to track the transformations, and experiment with different parameters while also fixing bugs and rerunning stuff. Because there is so much data and sometimes something crashes in the pipeline, I decided to save all intermediate results in a database in order to avoid repeating work.

Finally, I used Scala to generate corpora for importing into [The Sketch Engine](http://www.sketchengine.co.uk/) for our Farsi linguistics experts to analyze. Scala's parser combinator library came in very useful, and I represented each stage in the pipeline as a transformation of a custom AST designed to finally serialize well to the Sketch Engine "vert" file format.

It was pretty important to work with ASTs, since we actually got text in a variety of formats, which I parsed to a common AST in order to push into the pipeline.

Whew!

### (Update of 2014-06-10)

A description of some of this [METAL project](http://springfield.metaphor.cs.cmu.edu:8080/MetaphorViz/About.html) work is in the paper ["The CMU METAL Farsi NLP Approach"](http://www.lrec-conf.org/proceedings/lrec2014/pdf/596_Paper.pdf).
