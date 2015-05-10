---
layout: post
title: "Pittsburgh Code and Supply: Programming in journalism"
date: 2014-07-17T21:21:19-04:00
comments: true
categories:
- Pittsburgh Code and Supply
- Pittsburgh
- journalism
- data
- visualization
- Python
- Django
- DocumentCloud
- Bootstrap
- Git
- R
- SQL
- OpenRefine
- D3
---
{{< figure src="http://static.squarespace.com/static/538f3cc8e4b00f52172b5560/t/53a4ec76e4b0488fb14c5a29/1403317372203/sign.png?format=500w" >}}

I finally attended my first ever [Pittsburgh Code and Supply](http://www.codeandsupply.co/) meeting! The group, which posts its [events on Meetup](http://www.meetup.com/Pittsburgh-Code-Supply/), was created by local Pittsburgh software developer [Justin Reese](https://twitter.com/justinxreese) just a few months ago, in March 2014. His vision is to create a local community that, unlike conventional specific language/technology-based meetups, is much more universal and broad. If you haven't do so already, read more about the [goals of Pittsburgh Code and Supply](http://www.codeandsupply.co/about). I'm very excited about this new group!

The Code and Supply meetup I just attended was about ["Programming in journalism"](http://www.meetup.com/Pittsburgh-Code-Supply/events/186766512/). This is a topic of huge importance, and I was excited to attend to hear more about what some local journalist/programmers are doing.

<!--more-->

## A bit about sponsors

Justin opened by briefly saying a few words about the goals of Pittsburgh Code and Supply. He then noted that this month, the group did not have sponsors, unfortunately.

**Pittsburgh Code and Supply needs sponsors!**

If you like what Code and Supply is doing, please consider seeing if your employer would be willing to step up as a sponsor for the group.

## Contribute!

Also, in any case, spread the word about the group.

This is my first meeting, and next Monday, I am actually going to give my first presentation for the group, on ["Type-Directed TDD in Rust"](http://www.meetup.com/Pittsburgh-Code-Supply/events/183483622/). The group is always looking for people to speak about something or facilitate other formats of meetings, so [sign up](http://www.codeandsupply.co/speak/) if you have an idea! I personally have some ideas I'd like to propose for future meetings, such as discussions about technology-related topics such as philosophy, history, and education.

## AmyJo Brown

<iframe width="560" height="315" src="//www.youtube.com/embed/DaOxhNkHvvw" frameborder="0" allowfullscreen></iframe>

[AmyJo Brown](http://amyjobrown.com/), an independent journalist, gave a great talk about the kind of work that she does, and why it is important. She gave an overview of how journalism has changed because of the availability of data and because of the complexity of the world, both of which lead to the necessity of telling stories differently, as evidenced by what [ProPublica](http://www.propublica.org/), the [New York Times](http://www.nytimes.com/), and the [Los Angeles Times](http://www.latimes.com/) have been doing.

Her own work involves tracking political donations ("follow the money").

She gave examples of software tools she uses to get data, clean it, analyze it, and archive the whole process. On the programming end, she programs in Python, for example (Django for Web development), and uses [DocumentCloud](http://www.documentcloud.org/).

She talked about many things to keep in mind while doing this kind of data journalism. First of all, you have to know the limitations of the data you're getting. She has to deal with handwritten documents, and data in different formats. Data that is clearly incomplete or entered wrong (whether accidentally or maliciously). It is necessary to know the domain, know the context in which data was collected or required or submitted, to not fall into the trap of just blindly treating data as a record of the full truth and just get into programmatically processing it. There's a lot that involves human judgment calls. And this is why it is so important to save all the original information and document each step of data "cleaning" and interpretation.

Near the end of her talk, AmyJo had her colleague Katrina speak a bit also. Katrina was not originally trained as a programmer, but was always interested in politics. She ended up getting into programming later.

(I'm always very excited and impressed to hear of people with domain knowledge and passion who take up computer programming as a useful skill to apply to their domain. Sometimes I hear people saying "please don't code"; I totally disagree with this sentiment and believe that [everyone can benefit from learning the fundamentals of computer science and programming](http://franklinchen.com/blog/2011/12/09/why-everyone-should-learn-computer-science/).)

## Andrew McGill

<iframe width="560" height="315" src="//www.youtube.com/embed/0iT9rhwRXVs" frameborder="0" allowfullscreen></iframe>

[Andrew McGill](http://www.andrewrmcgill.com/) bills himself as "your friendly neighborhood journalist", and works for the [Pittsburgh Post-Gazette](http://www.post-gazette.com/).

He started by talking about his interesting career path, which involved being into computers before college, but then going to college and then finding other interests and abandoning programming, and then only later taking it back up, in the service of journalism, to enable him to find data and tell stories through visualization.

He showed us specific Web sites and articles he has created to provide better visualizations of data than what is available in raw form. For example, on the funny side, there is ["Where's Bill?"](http://newsinteractive.post-gazette.com/wheresbill/), which he created because he was not happy with the "official" boring text news feed of Mayor Bill Peduto's calendar. He scraped the official site with PHP. He also created a Bill Peduto Twitter bot using Python.

He also showed a D3-based visualization of ["The Mystery of the Plugged Wells"](http://newsinteractive.post-gazette.com/plugged-wells/). Again, the point was to take already available data but to tell a story better with it than just giving raw tables or a purely text-based narrative.

He showed some other projects, such as a visualization of school comparisons based on [PSSA](http://en.wikipedia.org/wiki/Pennsylvania_System_of_School_Assessment) data.

## Questions and answers

There were quite a lot of questions posed to the presenters throughout the evening, on the technologies used, on work with graphic designers, on the process of obtaining data, and political implications of the work. Great discussions.

## Technologies used

Here's something resembling a summary of technologies mentioned that the presenters use:

- Python
- Django
- DocumentCloud
- Bootstrap
- Git
- R
- SQL
- OpenRefine
- D3

## Interesting articles

Before the meetup, I posted to the [event page](http://www.meetup.com/Pittsburgh-Code-Supply/events/186766512/) a link to a provocative (perhaps too harsh and ranty) article warning about dangers in data journalism, [by visualization expert Alberto Cairo](http://www.niemanlab.org/2014/07/alberto-cairo-data-journalism-needs-to-up-its-own-standards/). It got no response.

Later, I also posted a link to a thoughtful examination of the question of [epistemology in data journalism](http://source.opennews.org/en-US/learning/true-facts-maybe/). I totally recommend reading that article.

AmyJo after the meetup posted some more links on the event page:

- ["Why I'm a newsroom coder"](https://medium.com/@jeremybowers/why-im-a-newsroom-coder-860d9d49f684)
- ["Planting the next crop of newsroom coders"](https://source.opennews.org/en-US/articles/planting-next-crop-newsroom-coders/)
- ["Data delayed is democracy denied"](http://www.nytimes.com/2014/07/17/opinion/the-fec-lags-on-campaign-finance-disclosures.html)

## Conclusion

I was very excited to attend my first Pittsburgh Code and Supply meetup, on the fascinating topic of programming in journalism. I felt I learned a lot about what is at stake and how programming is being used in journalism to better inform people of what is going on in the world.
